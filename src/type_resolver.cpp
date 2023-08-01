#include "type_resolver.hpp"

#include "fmt/core.h"

namespace cinnabar {

TypeResolver::TypeResolver(AST &ast) : ast(ast) {
  for (auto &name : default_builtin_types) {
    builtin_type_map[name] = ast.globals->get_type(name);
  }
  std::vector<std::string_view> numeric_type_names = {
      "i8",  "i16", "i32", "i64", "u8",   "u16",
      "u32", "u64", "f32", "f64", "char",
  };
  for (auto s : numeric_type_names) {
    builtin_numeric_types.insert(builtin_type_map[s]);
  }
}

TypeId TypeResolver::find_type(std::string_view name) {
  for (int i = namespaces.size() - 1; i >= 0; i--) {
    if (namespaces[i]->type_decls.contains(name)) {
      return namespaces[i]->type_decls[name];
    }
  }
  error(fmt::format("Could not find type {}", name));
}

// for now
TypeId TypeResolver::find_type(GenType type) {
  for (int i = namespaces.size() - 1; i >= 0; i--) {
    if (namespaces[i]->type_decls.contains(type.name)) {
      return namespaces[i]->type_decls[type.name];
    }
  }
  error(fmt::format("Could not find type {}", type.name));
}

TypeId
TypeResolver::find_binary_op(BinaryOp op, TypeId lhs_type, TypeId rhs_type) {
  if (builtin_numeric_types.contains(lhs_type) && builtin_numeric_types.contains(rhs_type)) {
    if (lhs_type != rhs_type) {
      error(fmt::format( "Invalid binary operation: {} {} {}\n", to_string(op), lhs_type, rhs_type));
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
      for (auto &p : decl->variants) {
        (void) p;
      }
      for (auto &fun : decl->methods) {
        resolve(*fun);
      }
    },
    [&](std::unique_ptr<FunDecl> &decl) {
      resolve(*decl);
    },
    [&](std::unique_ptr<StructDecl> &decl) {
      for (auto &p : decl->fields) {
        (void) p;
      }
      for (auto &fun : decl->methods) {
        resolve(*fun);
      }
    },
    [&](std::unique_ptr<VarDecl> &decl) {
      if (decl->type_specifier.has_value()) {
        decl->type = find_type(decl->type_specifier.value());
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
    [&](std::unique_ptr<DotRef> &) {},
    [&](std::unique_ptr<FunCall> &) {},
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
      VarDecl *var_decl = namespaces.back()->get_var(expr->name.str);
      if (var_decl == nullptr) {
        error(fmt::format("Could not find variable named {}", expr->name.str));
      }
      expr->type = var_decl->type;
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
    block.type = find_type("unit");
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
