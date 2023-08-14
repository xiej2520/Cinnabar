#include "type_resolver.hpp"

#include <ranges>

#include "fmt/core.h"

namespace cinnabar {

TypeResolver::TypeResolver(AST &ast) : ast(ast) {
  namespaces.push_back(ast.globals.get());
  for (auto &name : default_builtin_types) {
    builtin_type_map[name] = get_typeid(GenType(name));
  }
  namespaces.pop_back();
  std::vector<std::string_view> numeric_type_names = {
      "i8",  "i16", "i32", "i64", "u8",   "u16",
      "u32", "u64", "f32", "f64", "char",
  };
  for (auto s : numeric_type_names) {
    builtin_numeric_types.insert(builtin_type_map[s]);
  }
}

TypeId TypeResolver::get_typeid(const GenType &gentype) {
  std::string_view base_type = gentype.name;
  for (int i = namespaces.size() - 1; i >= 0; i--) {
    Namespace &namesp = *namespaces[i];
    if (namesp.names.contains(base_type)) {
      DeclPtr decl = namesp.names[base_type];
      auto gentype_str = gentype.to_string();
      // clang-format off
      return std::visit(overload{
        [&](BuiltinType *decl) {
          if (!namesp.concrete_types.contains(gentype_str)) {
            return namesp.concrete_types[gentype_str] = add_type(gentype, decl);
          }
          return namesp.concrete_types[gentype_str];
        },
        [&](EnumDecl *decl) {
          if (!namesp.concrete_types.contains(gentype_str)) {
            return namesp.concrete_types[gentype_str] = add_type(gentype, decl);
          }
          return namesp.concrete_types[gentype_str];
        },
        [&](FunDecl *) { error(fmt::format("Name {} is a function, not a type.", base_type)); return -1; },
        [&](StructDecl *decl) {
          if (!namesp.concrete_types.contains(gentype_str)) {
            return namesp.concrete_types[gentype_str] = add_type(gentype, decl);
          }
          return namesp.concrete_types[gentype_str];
        },
        [&](VarDecl *) { error(fmt::format("Name {} is a variable, not a type.", base_type)); return -1; },
      }, decl);
      // clang-format on
    }
  }
  error(fmt::format("Base type {} not found.", base_type));
}

const Type &TypeResolver::get_type(TypeId id) {
  if (id < 0 || static_cast<size_t>(id) >= ast.types.size()) {
    error(fmt::format("TypeId {} out of bounds", id));
  }
  return ast.types.at(id);
}


FunDecl *TypeResolver::get_fun(std::string_view name) {
  for (Namespace *namesp : namespaces | std::views::reverse) {
    if (namesp->names.contains(name)) {
      DeclPtr decl = namesp->names[name];
      if (!std::holds_alternative<FunDecl *>(decl)) {
        error(fmt::format("Name {} is not a function.", name));
      }
      return std::get<FunDecl *>(decl);
    }
  }
  error(fmt::format("Function {} not found.", name));
}

VarDecl *TypeResolver::get_var(std::string_view name) {
  for (int i = namespaces.size() - 1; i >= 0; i--) {
    Namespace &namesp = *namespaces[i];
    if (namesp.names.contains(name)) {
      DeclPtr decl = namesp.names[name];
      if (!std::holds_alternative<VarDecl *>(decl)) {
        error(fmt::format("Name {} is not a variable.", name));
      }
      return std::get<VarDecl *>(decl);
    }
  }
  error(fmt::format("Variable {} not found.", name));
}

DeclPtr TypeResolver::get_decl(std::string_view name) {
  for (Namespace *namesp : namespaces | std::views::reverse) {
    if (namesp->names.contains(name)) {
      return namesp->names[name];
    }
  }
  error(fmt::format("Name {} not found.", name));
}

TypeId TypeResolver::add_type(const GenType &type, TypeDeclPtr decl) {
  TypeId res = ast.types.size();
  ast.types.push_back(Type(decl, res, type));
  return res;
}

TypeId
TypeResolver::find_binary_op(BinaryOp op, TypeId lhs_type, TypeId rhs_type) {
  if (builtin_numeric_types.contains(lhs_type) && builtin_numeric_types.contains(rhs_type)) {
    if (lhs_type != rhs_type) {
      error(lhs_type, rhs_type);
    }
    switch (op) {
    case BinaryOp::ADD:
    case BinaryOp::SUB:
    case BinaryOp::MUL:
    case BinaryOp::DIV:
    case BinaryOp::MOD:
    case BinaryOp::BIT_AND:
    case BinaryOp::BIT_OR:
    case BinaryOp::BIT_XOR:
    case BinaryOp::LEFT_SHIFT:
    case BinaryOp::RIGHT_SHIFT:
      return lhs_type;
    case BinaryOp::EQ:
    case BinaryOp::NEQ:
    case BinaryOp::GT:
    case BinaryOp::GTE:
    case BinaryOp::LTE:
      return builtin_type_map["bool"];
    default:;
    }
  }
  error(fmt::format(
      "Invalid binary operation: {} {} {}\n", to_string(op), lhs_type, rhs_type
  ));
}

// clang-format off
void TypeResolver::resolve(Stmt &stmt) {
  std::visit(overload {
    [&](std::monostate){},
    [&](std::unique_ptr<Assign> &stmt) {
      // check for assignable
      if (stmt->lhs.is<std::unique_ptr<Variable>>()) {
        // assign to variable
      }
      else if (stmt->lhs.is<std::unique_ptr<DotRef>>()) {
        // field
      }
      else if (stmt->lhs.is<std::unique_ptr<FunCall>>()) {
        // only deref builtin for now
        FunCall &fun_call = *stmt->lhs.as<std::unique_ptr<FunCall>>();
        if (fun_call.callee.is<std::unique_ptr<Variable>>()) {
          Variable &fun = *fun_call.callee.as<std::unique_ptr<Variable>>();
          if (fun.name.str != "__deref") {
            error("Left side of assignment is a call to function, but not __deref.");
          }
        }
      }
      else {
        error("Left side of assignment is not an assignee expression.");
      }

      resolve(stmt->lhs);
      resolve(stmt->rhs);
      if (stmt->lhs.type() != stmt->rhs.type()) {
        error(fmt::format("Left side {} has type {}, does not match right {} side type {}.",
            stmt->lhs.s_expr(0, 2), stmt->lhs.type(), stmt->rhs.s_expr(0, 2), stmt->rhs.type()));
      }
    },
    [&](std::unique_ptr<Break> &) {},
    [&](std::unique_ptr<Continue> &) {},
    [&](Declaration &stmt) {
      resolve(stmt);
    },
    [&](std::unique_ptr<Expression> &stmt) {
      resolve(stmt->expr);
    },
    [&](std::unique_ptr<For> &) {},
    [&](std::unique_ptr<Return> &) {},
    [&](std::unique_ptr<While> &) {},
  }, stmt.node);
}

void TypeResolver::resolve(Declaration &decl) {
  std::visit(overload{
    [&](std::unique_ptr<EnumDecl> &decl) {
      get_typeid(GenType{decl->name.str}); // remove later???

      for (auto &variant : decl->variants) {
        variant.type = get_typeid(variant.gentype);
      }
      for (auto &fun : decl->methods) {
        resolve(*fun);
        ast.fun_ptrs.push_back(fun.get());
      }
    },
    [&](std::unique_ptr<FunDecl> &decl) {
      resolve(*decl);
      ast.fun_ptrs.push_back(decl.get());
    },
    [&](std::unique_ptr<StructDecl> &decl) {
      get_typeid(GenType{decl->name.str}); // remove later???
      for (auto &field : decl->fields) {
        field.type = get_typeid(field.gentype);
      }
      for (auto &fun : decl->methods) {
        resolve(*fun);
        ast.fun_ptrs.push_back(fun.get());
      }
    },
    [&](std::unique_ptr<VarDecl> &decl) {
      if (decl->type_specifier.has_value()) {
        decl->type = get_typeid(decl->type_specifier.value());
        if (!decl->initializer.has_value()) {
          resolve(decl->initializer.value());
          if (decl->initializer.value().type() != decl->type) {
            error(fmt::format("Variable {} has declared type {}, does not match initializer type {}",
                decl->name.str, decl->type, decl->initializer.value().type()));
          }
        }
      }
      else if (decl->initializer.has_value()) {
        resolve(decl->initializer.value());
        decl->type = decl->initializer.value().type();
      }
      else {
        error(fmt::format("Variable {} does not have a declared type or initializer"
            " (currently do not support type inference).", decl->name.str));
      }
    },
  }, decl.decl);
}

void TypeResolver::resolve(FunDecl &fun) {
  resolve(*fun.body);
}

void TypeResolver::resolve(Expr &expr) {
  std::visit(overload {
    [&](std::unique_ptr<Binary> &expr) {
      resolve(expr->left);
      resolve(expr->right);
      
      expr->type = find_binary_op(expr->op, expr->left.type(), expr->right.type());
    },
    [&](std::unique_ptr<Block> &expr) { resolve(*expr); },
    [&](std::unique_ptr<DotRef> &expr) {
      resolve(expr->lvalue);
      const Type &t = get_type(expr->lvalue.type());
      expr->type = std::visit(overload{
        [&](BuiltinType *) {
          return builtin_type_map["unit"]; // for now
        },
        [&](EnumDecl *) {
          return builtin_type_map["unit"]; // for now
        },
        [&](StructDecl *decl) {
          for (auto &field : decl->fields) {
            if (field.name.str == expr->name.str) {
              return get_typeid(field.gentype);
            }
          }
          for (auto &fun_decl : decl->methods) {
            if (fun_decl->name.str == expr->name.str) {
              expr->fun_decl = fun_decl.get();
              return builtin_type_map["__fun"];
            }
          }
          error(fmt::format("Name {} in dot ref not found.", expr->name.str));
        }
      }, t.type_decl_ptr);
    },
    [&](std::unique_ptr<FunCall> &expr) {
      resolve(expr->callee);
      if (expr->callee.type() != builtin_type_map["__fun"]) {
        error("Type of callee was not a function.");
      }

      FunDecl *fun_decl = nullptr;
      // functions and struct constructors callable
      if (expr->callee.is<std::unique_ptr<Variable>>()) {
        Variable &var = *expr->callee.as<std::unique_ptr<Variable>>();
        std::visit(overload{
          [&](BuiltinType *) { error("Builtin type not callable."); },
          [&](EnumDecl *) { error("Enum type not callable."); },
          [&](FunDecl *decl) {
            if (decl == nullptr) error("Internal error, declaration not found.");
            expr->type = get_typeid(decl->return_type);
            fun_decl = decl;
          },
          [&](StructDecl *) {
            error("Struct not callable");
          },
          [&](VarDecl *) { error("Variable not callable."); },
        }, var.decl);
      }
      // dotref to function callable
      else if (expr->callee.is<std::unique_ptr<DotRef>>()) {
        DotRef &dot = *expr->callee.as<std::unique_ptr<DotRef>>();
        if (dot.fun_decl == nullptr) {
          error("Field is not callable");
        }
        fun_decl = dot.fun_decl;
        expr->type = get_typeid(dot.fun_decl->return_type);
      }
      else {
        error("Callee is not callable.");
      }
      
      // check arity
      if (fun_decl->params.size() != expr->args.size()) {
        error("Function declaration has different arity than function call.");
      }
      // check function type match
      for (size_t i=0; i<expr->args.size(); i++) {
        resolve(expr->args[i]);
        if (expr->args[i].type() != fun_decl->params[i]->type) {
          error(fun_decl->params[i]->type, expr->args[i].type());
        }
      }
    },
    [&](std::unique_ptr<If> &expr) {
      for (auto &branch : expr->branches) {
        resolve(branch->condition);
        if (branch->condition.type() != builtin_type_map["bool"]) {
          error(builtin_type_map["bool"], branch->condition.type());
        }
        resolve(*branch->block);
      }
      expr->type = expr->branches[0]->block->type;
      for (size_t i=1; i<expr->branches.size(); i++) {
        if (expr->type != expr->branches[i]->block->type) {
          expr->type = builtin_type_map["unit"];
          break;
        }
      }
    },
    [&](std::unique_ptr<Literal> &expr) {
      expr->type = std::visit(overload{
        [&](i32) { return builtin_type_map["i32"]; },
        [&](i64) { return builtin_type_map["i64"]; },
        [&](f32) { return builtin_type_map["f32"]; },
        [&](f64) { return builtin_type_map["f64"]; },
        [&](bool) { return builtin_type_map["bool"]; },
        [&](char) { return builtin_type_map["char"]; },
        [&](std::string) { return builtin_type_map["Slice[char]"]; },
      }, expr->val);
    },
    [&](std::unique_ptr<Unary> &) {},
    [&](std::unique_ptr<Variable> &expr) {
      // "variable" is identifier, can be var, fun, struct, enum
      DeclPtr decl = get_decl(expr->name.str);
      if (FunDecl **fun_decl = std::get_if<FunDecl *>(&decl)) {
        expr->type = get_typeid(GenType{"__fun"});
        expr->decl = *fun_decl;
      }
      else if (VarDecl **var_decl = std::get_if<VarDecl *>(&decl)) {
        expr->type = (*var_decl)->type;
        expr->decl = *var_decl;
      }
      else {
        error(fmt::format("Could not find variable named {}", expr->name.str));
      }
    },
  }, expr.node);
}

void TypeResolver::resolve(Block &block) {
  namespaces.push_back(block.namesp.get());
  
  for (Stmt &stmt : block.stmts) {
    resolve(stmt);
  }

  namespaces.pop_back();
  
  if (!block.stmts.empty() && block.stmts.back().is<std::unique_ptr<Expression>>()) {
    block.type = block.stmts.back().as<std::unique_ptr<Expression>>()->expr.type();
  }
  else {
    block.type = get_typeid(GenType{"unit"});
  }
}
// clang-format on

void TypeResolver::resolve() {
  namespaces.push_back(ast.globals.get());
  for (Declaration &decl : ast.decls) {
    resolve(decl);
  }
}

[[noreturn]] void TypeResolver::error(std::string_view message) {
  fmt::print(stderr, "Type Resolution Error: {}\n", message);
  abort();
}

[[noreturn]] void TypeResolver::error(TypeId expected, TypeId found) {
  error(fmt::format("Expected type {}, found type {}.", ast.types[expected].name(),
      ast.types[found].name()));
}

} // namespace cinnabar
