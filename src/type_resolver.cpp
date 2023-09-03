#include "type_resolver.hpp"

#include <functional>
#include <ranges>

#include "fmt/core.h"

namespace cinnabar {

TypeResolver::PushNamespace
TypeResolver::push_namespace(Namespace *namesp, TNamespace *tnamesp) {
  return PushNamespace(*this, namesp, tnamesp);
}

TypeResolver::TypeResolver(AST &ast) : ast(ast) {}

TypeId TypeResolver::get_typeid(const GenericInst &gentype) {
  std::string_view base_type = gentype.base_name;

  auto namesp = cur_ast_namesp;
  auto tnamesp = cur_tnamesp;
  for (; namesp != nullptr;
       namesp = namesp->parent, tnamesp = tnamesp->parent) {
    if (namesp->names.contains(base_type)) {
      DeclPtr decl = namesp->names[base_type];
      auto gentype_str = gentype.to_string();
      // clang-format off
      return std::visit(overload{
        [&](BuiltinType *decl) {
          if (!tnamesp->concrete_types.contains(gentype_str)) {
            return tnamesp->concrete_types[gentype_str] = add_type(gentype, decl, tnamesp);
          }
          return tnamesp->concrete_types[gentype_str];
        },
        [&](EnumDecl *decl) {
          if (!tnamesp->concrete_types.contains(gentype_str)) {
            return tnamesp->concrete_types[gentype_str] = add_type(gentype, decl, tnamesp);
          }
          return tnamesp->concrete_types[gentype_str];
        },
        [&](FunDecl *) { error(fmt::format("Name {} is a function, not a type.", base_type)); return TypeId{-1}; },
        [&](StructDecl *decl) {
          if (!tnamesp->concrete_types.contains(gentype_str)) {
            return tnamesp->concrete_types[gentype_str] = add_type(gentype, decl, tnamesp);
          }
          return tnamesp->concrete_types[gentype_str];
        },
        [&](VarDecl *) { error(fmt::format("Name {} is a variable, not a type.", base_type)); return TypeId{-1}; },
      }, decl);
      // clang-format on
    }
  }
  error(fmt::format("Base type {} not found.", base_type));
}

const TTypeInst &TypeResolver::get_type(TypeId id) {
  if (id < 0 || static_cast<size_t>(id) >= types.size()) {
    error(fmt::format("TypeId {} out of bounds", id.value));
  }
  return types.at(id);
}

FunId TypeResolver::get_funid(const GenericInst &gentype) {
  std::string_view base_fun = gentype.base_name;

  auto namesp = cur_ast_namesp;
  auto tnamesp = cur_tnamesp;
  for (; namesp != nullptr;
       namesp = namesp->parent, tnamesp = tnamesp->parent) {
    if (namesp->names.contains(base_fun)) {
      DeclPtr decl = namesp->names[base_fun];
      if (!std::holds_alternative<FunDecl *>(decl)) {
        error(fmt::format("Name {} is not a function.", base_fun));
      }
      auto gentype_str = gentype.to_string();
      if (!tnamesp->concrete_funs.contains(gentype_str)) {
        return tnamesp->concrete_funs[gentype_str] =
                   add_fun(gentype, std::get<FunDecl *>(decl), tnamesp);
      }
      return tnamesp->concrete_funs[gentype_str];
    }
  }
  error(fmt::format("Function {} not found.", base_fun));
}

const TFunInst &TypeResolver::get_fun(FunId id) {
  if (id < 0 || static_cast<size_t>(id) >= functions.size()) {
    error(fmt::format("FunId {} out of bounds", id.value));
  }
  return functions.at(id);
}

TVarInst *TypeResolver::get_var(std::string_view name) {
  auto tnamesp = cur_tnamesp;
  for (; tnamesp != nullptr; tnamesp = tnamesp->parent) {
    if (tnamesp->variables.contains(name)) {
      return tnamesp->variables[name];
    }
  }
  error(fmt::format("Variable {} not found.", name));
}

DeclaredName TypeResolver::get_decl(const GenericInst &geninst) {
  std::string_view base_name = geninst.base_name;

  auto namesp = cur_ast_namesp;
  auto tnamesp = cur_tnamesp;

  std::string geninst_str = geninst.to_string();
  for (; namesp != nullptr;
       namesp = namesp->parent, tnamesp = tnamesp->parent) {
    if (namesp->names.contains(base_name)) {
      DeclPtr decl = namesp->names[base_name];
      return std::visit(
          overload{
              [&](BuiltinType *decl) -> DeclaredName {
                if (!tnamesp->concrete_types.contains(geninst_str)) {
                  return tnamesp->concrete_types[geninst_str] =
                             add_type(geninst, decl, tnamesp);
                }
                return tnamesp->concrete_types[geninst_str];
              },
              [&](EnumDecl *decl) -> DeclaredName {
                if (!tnamesp->concrete_types.contains(geninst_str)) {
                  return tnamesp->concrete_types[geninst_str] =
                             add_type(geninst, decl, tnamesp);
                }
                return tnamesp->concrete_types[geninst_str];
              },
              [&](FunDecl *) -> DeclaredName {
                if (!tnamesp->concrete_funs.contains(geninst_str)) {
                  return tnamesp->concrete_funs[geninst_str] = add_fun(
                             geninst, std::get<FunDecl *>(decl), tnamesp
                         );
                }
                return tnamesp->concrete_funs[geninst_str];
              },
              [&](StructDecl *decl) -> DeclaredName {
                if (!tnamesp->concrete_types.contains(geninst_str)) {
                  return tnamesp->concrete_types[geninst_str] =
                             add_type(geninst, decl, tnamesp);
                }
                return tnamesp->concrete_types[geninst_str];
              },
              [&](VarDecl *) -> DeclaredName {
                if (!geninst.is_concrete()) {
                  error("Internal error: Tried to search for variable with "
                        "type parameter?");
                } else if (!tnamesp->variables.contains(base_name)) {
                  error(fmt::format(
                      "Variable {} used before definition complete.", base_name
                  ));
                }
                return tnamesp->variables[base_name];
              },
          },
          decl
      );
    }
  }
  error(fmt::format("Base name {} not found.", base_name));
}

TypeId TypeResolver::add_type(
    const GenericInst &type, TypeDeclPtr decl, TNamespace *parent_tnamesp
) {
  auto type_name = type.to_string();
  if (currently_creating.contains(type_name)) {
    error(fmt::format(
        "Cycle detected while instantiating types at {}.", type_name
    ));
  }
  currently_creating.insert(type_name);

  TypeId res = types.size();
  // clang-format off
  std::visit(overload{
    [&](BuiltinType *decl) {
      parent_tnamesp->concrete_types[type_name] = res;
      types.push_back(TTypeInst{*decl});
    },
    [&](EnumDecl *decl) {
      types.push_back(TTypeInst{TEnumInst{type, {}, std::make_unique<TNamespace>(parent_tnamesp), {}, {}}});
      // don't use reference, types will get resized
      auto p = push_namespace(decl->namesp.get(), types[res].as<TEnumInst>().namesp.get());

      parent_tnamesp->concrete_types[type_name] = res;
      types[res].as<TEnumInst>().namesp->concrete_types[type_name] = res;

      for (size_t i = 0; i < type.args.size(); i++) {
        TypeId genparam_id = get_typeid(type.args[i]);
        types[res].as<TEnumInst>().generic_args.push_back(genparam_id);
        types[res].as<TEnumInst>().namesp->concrete_types[decl->name_param.params[i]] = genparam_id;
      }
      
      for (size_t i = 0; i < decl->variants.size(); i++) {
        auto &[name, gentype] = decl->variants[i];
        types[res].as<TEnumInst>().variants[name.str] = {get_typeid(gentype), i};
      }
      
      // add methods? later

    },
    [&](StructDecl *decl) {
      types.push_back(TTypeInst{TStructInst{type, {}, std::make_unique<TNamespace>(parent_tnamesp), {}, {}}});
      auto p = push_namespace(decl->namesp.get(), types[res].as<TStructInst>().namesp.get());

      parent_tnamesp->concrete_types[type_name] = res;
      types[res].as<TStructInst>().namesp->concrete_types[type_name] = res;

      for (size_t i = 0; i < type.args.size(); i++) {
        TypeId genparam_id = get_typeid(type.args[i]);
        types[res].as<TStructInst>().generic_args.push_back(genparam_id);
        types[res].as<TStructInst>().namesp->concrete_types[decl->name_param.params[i]] = genparam_id;
      }
      
      for (size_t i = 0; i < decl->fields.size(); i++) {
        auto &[name, gentype] = decl->fields[i];
        types[res].as<TStructInst>().fields[name.str] = {get_typeid(gentype), i};
      }
      
      // add methods? later
    },
  }, decl);
  // clang-format on
  currently_creating.erase(type_name);
  parent_tnamesp->concrete_types[type_name] = res;
  return res;
}

FunId TypeResolver::add_fun(
    const GenericInst &fun, FunDecl *decl, TNamespace *parent_tnamesp
) {
  FunId res = functions.size();

  functions.push_back(TFunInst{fun, {}, {}, -1, nullptr});
  // functions changes size - do not take reference

  auto block_namesp = std::make_unique<TNamespace>(parent_tnamesp);
  parent_tnamesp->concrete_funs[fun.to_string()] = res;
  block_namesp->concrete_funs[fun.to_string()] = res;

  for (size_t i = 0; i < fun.args.size(); i++) {
    TypeId genparam_id = get_typeid(fun.args[i]);
    functions[res].generic_args.push_back(genparam_id);
    block_namesp->concrete_types[decl->name_param.params[i]] = genparam_id;
  }

  auto p = push_namespace(decl->body->namesp.get(), block_namesp.get());

  for (size_t i = 0; i < decl->params.size(); i++) {
    functions[res].params.push_back(resolve(*decl->params[i]));
  }

  functions[res].return_type = get_typeid(decl->return_type);

  functions[res].body = resolve(*decl->body, std::move(block_namesp));

  return res;
}

TypeId TypeResolver::find_unary_op(UnaryOp op, TypeId operand_type) {
  switch (op) {
  case UnaryOp::PLUS:
  case UnaryOp::NEG:
    if (!builtin_numeric_types.contains(operand_type)) {
      error(fmt::format(
          "Invalid unary operation: {} {}\n", to_string(op), operand_type.value
      ));
    }
    return operand_type;
  case UnaryOp::NOT:
    if (operand_type != builtin_type_map["bool"]) {
      error(fmt::format(
          "Invalid unary operation: {} {}\n", to_string(op), operand_type.value
      ));
    }
    return operand_type;
  case UnaryOp::REF:
  case UnaryOp::DEREF:
  default:
    error(fmt::format(
        "Invalid unary operation: {} {}\n", to_string(op), operand_type.value
    ));
  }
}

TypeId
TypeResolver::find_binary_op(BinaryOp op, TypeId lhs_type, TypeId rhs_type) {
  if (builtin_numeric_types.contains(lhs_type) &&
      builtin_numeric_types.contains(rhs_type)) {
    if (lhs_type != rhs_type) {
      error(lhs_type, rhs_type);
    }
    switch (op) {
    case BinaryOp::ADD:
    case BinaryOp::SUB:
    case BinaryOp::MUL:
    case BinaryOp::DIV:
    case BinaryOp::MOD:
    case BinaryOp::XOR:
    case BinaryOp::BIT_AND:
    case BinaryOp::BIT_OR:
    case BinaryOp::LEFT_SHIFT:
    case BinaryOp::RIGHT_SHIFT:
      return lhs_type;
    case BinaryOp::EQ:
    case BinaryOp::NEQ:
    case BinaryOp::LT:
    case BinaryOp::GT:
    case BinaryOp::GTE:
    case BinaryOp::LTE:
      return builtin_type_map["bool"];
    default:;
    }
  }
  else if (lhs_type == builtin_type_map["bool"] && rhs_type == builtin_type_map["bool"]) {
    switch (op) {
    case BinaryOp::AND:
    case BinaryOp::OR:
    case BinaryOp::XOR:
      return builtin_type_map["bool"];
    default:;
    }
  }
  error(fmt::format(
      "Invalid binary operation: {} {} {}\n", to_string(op), lhs_type.value,
      rhs_type.value
  ));
}

// clang-format off
std::optional<TStmt> TypeResolver::resolve(Stmt &stmt) {
  return std::visit(overload{
    [&](std::monostate) -> std::optional<TStmt>  { return std::nullopt; },
    [&](std::unique_ptr<Assign> &stmt) -> std::optional<TStmt>  {
      auto res = std::make_unique<TAssign>();
      res->op = stmt->op;
      res->lhs = resolve(stmt->lhs);
      res->rhs = resolve(stmt->rhs);

      // check for assignable
      if (res->lhs.is<std::unique_ptr<TVariable>>()) {
        // assign to variable
      }
      else if (res->lhs.is<std::unique_ptr<TDotRef>>()) {
        // field
      }
      else if (res->lhs.is<std::unique_ptr<TFunCall>>()) {
        // only deref builtin for now
        TFunCall &fun_call = *res->lhs.as<std::unique_ptr<TFunCall>>();
        if (fun_call.callee.is<std::unique_ptr<TVariable>>()) {
          TVariable &fun = *fun_call.callee.as<std::unique_ptr<TVariable>>();
          if (fun.name.str != "__deref") {
            error("Left side of assignment is a call to function, but not __deref.");
          }
        }
      }
      else {
        error("Left side of assignment is not an assignee expression.");
      }

      if (res->lhs.type() != res->rhs.type()) {
        error(fmt::format("Left side {} has type {}, does not match right side {} type {}.",
            stmt->lhs.s_expr(0, 2), res->lhs.type().value, stmt->rhs.s_expr(0, 2), res->rhs.type().value));
      }
      return TStmt{std::move(res)};
    },
    [&](std::unique_ptr<Break> &) -> std::optional<TStmt>  { return TStmt{std::make_unique<TBreak>()}; },
    [&](std::unique_ptr<Continue> &) -> std::optional<TStmt>  { return TStmt{std::make_unique<TContinue>()}; },
    [&](Declaration &stmt) -> std::optional<TStmt> {
      auto res = resolve(stmt);
      if (res.has_value()) {
        return TStmt{std::move(res.value())};
      }
      return std::nullopt;
    },
    [&](std::unique_ptr<Expression> &stmt) -> std::optional<TStmt>  {
      return TStmt{resolve(stmt->expr)};
    },
    [&](std::unique_ptr<For> &) -> std::optional<TStmt>  { return std::nullopt; },
    [&](std::unique_ptr<Return> &) -> std::optional<TStmt>  { return std::nullopt; },
    [&](std::unique_ptr<While> &) -> std::optional<TStmt>  { return std::nullopt; },
  }, stmt.node);
}

std::optional<std::unique_ptr<TVarInst>> TypeResolver::resolve(Declaration &decl) {
  return std::visit(overload{
    [&](std::unique_ptr<EnumDecl> &decl) -> std::optional<std::unique_ptr<TVarInst>> {
      if (decl->name_param.is_concrete()) {
        get_typeid(GenericInst{decl->name_param.base_name});
      }
      return std::nullopt;
    },
    [&](std::unique_ptr<FunDecl> &decl) -> std::optional<std::unique_ptr<TVarInst>> {
      if (decl->name_param.is_concrete()) {
        get_funid(GenericInst{decl->name_param.base_name});
      }
      return std::nullopt;
    },
    [&](std::unique_ptr<StructDecl> &decl) -> std::optional<std::unique_ptr<TVarInst>> {
      if (decl->name_param.is_concrete()) {
        get_typeid(GenericInst{decl->name_param.base_name});
      }
      return std::nullopt;
    },
    [&](std::unique_ptr<VarDecl> &decl) -> std::optional<std::unique_ptr<TVarInst>> {
      return resolve(*decl);
    },
  }, decl.decl);
}

std::unique_ptr<TVarInst> TypeResolver::resolve(VarDecl &decl) {
  auto res = std::make_unique_for_overwrite<TVarInst>();
  res->name = decl.name;

  if (decl.initializer.has_value()) {
    res->initializer = resolve(decl.initializer.value());
    res->type = res->initializer->type();
  }
  else {
    res->initializer = std::nullopt;
    res->type = -1;
  }
  if (decl.type_specifier.has_value()) {
    auto specifier_type = get_typeid(decl.type_specifier.value());
    if (res->initializer.has_value() && res->type != specifier_type) {
        error(fmt::format("Variable {} has declared type {}, does not match initializer type {}",
            res->name.str, specifier_type.value, res->type.value));
    }
    res->type = specifier_type;
  }
  if (res->type == -1) {
    error(fmt::format("Variable {} does not have a declared type or initializer"
        " (currently do not support type inference).", res->name.str));
  }
  
  cur_tnamesp->variables[res->name.str] = res.get();
  return res;
}

TExpr TypeResolver::resolve(Expr &expr) {
  return std::visit(overload{
    [&](std::unique_ptr<Binary> &expr) {
      auto res = std::make_unique_for_overwrite<TBinary>();
      res->op = expr->op;
      res->left = resolve(expr->left);
      res->right = resolve(expr->right);
      
      res->type = find_binary_op(expr->op, res->left.type(), res->right.type());
      return TExpr{std::move(res)};
    },
    [&](std::unique_ptr<Block> &expr) { return TExpr{resolve(*expr)}; },
    [&](std::unique_ptr<DotRef> &expr) {
      auto res = std::make_unique_for_overwrite<TDotRef>();
      res->left = resolve(expr->lvalue);
      res->name = expr->name;

      const TTypeInst &t = get_type(res->left.type());
      res->type = std::visit(overload{
        [&](const BuiltinType &bt) {
          return builtin_type_map[bt.name.str];
        },
        [&](const TEnumInst &) {
          return builtin_type_map["unit"]; // for now
        },
        [&](const TStructInst &inst) {
          if (auto it = inst.fields.find(res->name.str); it != inst.fields.end()) {
            res->prop_idx = it->second.second;
            return it->second.first;
          }
          else if (auto it = inst.methods.find(res->name.str); it != inst.methods.end()) {
            return builtin_type_map["__fun"];
          }
          error(fmt::format("Name {} in dot ref not found.", expr->name.str));
        }
      }, t.def);
      
      return TExpr{std::move(res)};
    },
    [&](std::unique_ptr<FunCall> &expr) {
      auto res = std::make_unique<TFunCall>();
      res->callee = resolve(expr->callee);

      if (res->callee.type() != builtin_type_map["__fun"]) {
        error("Type of callee was not a function.");
      }

      // functions and struct constructors callable
      // find the function being called
      if (res->callee.is<std::unique_ptr<TVariable>>()) {
        TVariable &var = *res->callee.as<std::unique_ptr<TVariable>>();
        std::visit(overload{
          //[&](BuiltinType *) { error("Builtin type not callable."); },
          //[&](EnumDecl *) { error("Enum type not callable."); },
          [&](FunId id) {
            if (id < 0) error("Internal error, declaration not found.");
            res->type = get_fun(id).return_type;
            res->fun = id;
          },
          [&](TypeId) { error("Type not callable."); },
          //[&](StructDecl *) { error("Struct not callable"); },
          [&](TVarInst *) { error("Variable not callable."); },
        }, var.decl);
      }
      // dotref to function callable (method)
      else if (res->callee.is<std::unique_ptr<TDotRef>>()) {
        TDotRef &dot = *res->callee.as<std::unique_ptr<TDotRef>>();
        if (dot.fun == -1) {
          error("Field is not callable");
        }
        res->fun = dot.fun;
        res->type = get_fun(res->fun).return_type;
      }
      else {
        error("Callee is not callable.");
      }
      
      // check arity
      auto &fun_inst = get_fun(res->fun);
      if (fun_inst.params.size() != expr->args.size()) {
        error("Function declaration has different arity than function call.");
      }
      // check function type match
      for (size_t i=0; i < expr->args.size(); i++) {
        res->args.push_back(resolve(expr->args[i]));

        if (res->args[i].type() != fun_inst.params[i]->type) {
          error(fun_inst.params[i]->type, res->args[i].type());
        }
      }

      return TExpr{std::move(res)};
    },
    [&](std::unique_ptr<If> &expr) {
      auto res = std::make_unique<TIf>();

      for (auto &branch : expr->branches) {
        res->branches.push_back(std::make_unique<TIf::TBranch>());
        auto &tbranch = *res->branches.back();

        tbranch.condition = resolve(branch->condition);
        if (tbranch.condition.type() != builtin_type_map["bool"]) {
          error(builtin_type_map["bool"], tbranch.condition.type());
        }
        tbranch.block = resolve(*branch->block);
      }

      if (!res->has_else()) {
        res->type = builtin_type_map["unit"];
      }
      else {
        res->type = res->branches.front()->block->type;
        for (size_t i = 1; i < res->branches.size(); i++) {
          if (res->type != res->branches[i]->block->type) {
            res->type = builtin_type_map["unit"];
            break;
          }
        }
      }

      return TExpr{std::move(res)};
    },
    [&](std::unique_ptr<Literal> &expr) {
      auto res = std::make_unique_for_overwrite<TLiteral>();

      res->val = expr->val;

      res->type = std::visit(overload{
        [&](int32_t) { return builtin_type_map["i32"]; },
        [&](int64_t) { return builtin_type_map["i64"]; },
        [&](float)   { return builtin_type_map["f32"]; },
        [&](double)  { return builtin_type_map["f64"]; },
        [&](bool)    { return builtin_type_map["bool"]; },
        [&](char)    { return builtin_type_map["char"]; },
        [&](std::string) { return builtin_type_map["Span[char]"]; },
      }, res->val);
      
      return TExpr{std::move(res)};
    },
    [&](std::unique_ptr<Unary> &expr) {
      auto res = std::make_unique_for_overwrite<TUnary>();
      res->op = expr->op;
      res->operand = resolve(expr->operand);
      
      res->type = find_unary_op(expr->op, res->operand.type());
      return TExpr{std::move(res)};
    },
    [&](std::unique_ptr<Variable> &expr) {
      // "variable" is identifier, can be var, fun, struct, enum
      auto res = std::make_unique_for_overwrite<TVariable>();

      res->name = expr->name;
      res->decl = get_decl(expr->name.str);
      if (FunId *id = std::get_if<FunId>(&res->decl)) {
        res->type = builtin_type_map["__fun"];
      }
      else if (TVarInst **var_inst = std::get_if<TVarInst *>(&res->decl)) {
        res->type = (*var_inst)->type;
      }
      else {
        error(fmt::format("Could not find variable named {}", expr->name.str));
      }
      
      return TExpr{std::move(res)};
    },
  }, expr.node);
  // clang-format on
}

std::unique_ptr<TBlock>
TypeResolver::resolve(Block &block, std::unique_ptr<TNamespace> tnamesp) {
  auto res = std::make_unique<TBlock>();
  res->namesp = std::move(tnamesp);
  auto p = push_namespace(block.namesp.get(), res->namesp.get());

  for (Stmt &stmt : block.stmts) {
    auto tstmt = resolve(stmt);
    if (tstmt.has_value()) {
      res->stmts.push_back(std::move(tstmt.value()));
    }
  }

  if (!res->stmts.empty() && res->stmts.back().is<TExpr>()) {
    res->type = res->stmts.back().as<TExpr>().type();
  } else {
    res->type = builtin_type_map["unit"];
  }
  return res;
}

std::unique_ptr<TBlock> TypeResolver::resolve(Block &block) {
  auto tnamesp = std::make_unique<TNamespace>(cur_tnamesp);
  return resolve(block, std::move(tnamesp));
}

TAST TypeResolver::resolve() {
  builtin_numeric_types.clear();
  functions.clear();
  types.clear();
  builtin_types.clear();
  builtin_type_map.clear();

  cur_ast_namesp = ast.globals.get();
  auto root_namesp = std::make_unique<TNamespace>(nullptr);
  cur_tnamesp = root_namesp.get();

  TAST tast;

  std::vector<std::unique_ptr<TVarInst>> globals;

  for (auto &name : default_builtin_types) {
    builtin_type_map[name] = get_typeid(GenericInst{name});
  }

  std::vector<std::string_view> numeric_type_names = {
      "i8",  "i16", "i32", "i64", "u8",   "u16",
      "u32", "u64", "f32", "f64", "char",
  };
  for (auto s : numeric_type_names) {
    builtin_numeric_types.insert(builtin_type_map[s]);
  }

  for (Declaration &decl : ast.decls) {
    auto res = resolve(decl);
    if (res.has_value()) {
      globals.push_back(std::move(res.value()));
    }
  }

  tast.functions = std::move(functions);
  tast.types = std::move(types);
  tast.globals = std::move(globals);
  tast.root_namesp = std::move(root_namesp);
  tast.builtin_type_map = std::move(builtin_type_map);

  return tast;
}

[[noreturn]] void TypeResolver::error(std::string_view message) {
  fmt::print(stderr, "Type Resolution Error: {}\n", message);
  abort();
}

[[noreturn]] void TypeResolver::error(TypeId expected, TypeId found) {
  error(fmt::format(
      "Expected type {}, found type {}.", types[expected].name(),
      types[found].name()
  ));
}

} // namespace cinnabar
