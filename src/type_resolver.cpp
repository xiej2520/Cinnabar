#include "type_resolver.hpp"

#include <functional>
#include <ranges>
#include <string>

#include "fmt/core.h"

namespace cinnabar {

const std::vector<std::string_view> numeric_primitive_names = {
    "i8",  "i16", "i32", "i64",  "u8",    "u16",   "u32",
    "u64", "f32", "f64", "char", "isize", "usize",
};

TypeResolver::PushNamespace
TypeResolver::push_namespace(Namespace *namesp, TNamespace *tnamesp) {
  return {this, namesp, tnamesp};
}
TypeResolver::PushFun
TypeResolver::push_fun(TFunInst *fun, TNamespace *fun_tnamesp) {
  return {this, fun, fun_tnamesp};
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
        [&](Primitive *decl) {
          if (auto it = tnamesp->concrete_types.find(gentype_str); it != tnamesp->concrete_types.end()) {
            return it->second;
          }
          return tnamesp->concrete_types[gentype_str] = add_type(gentype, decl, tnamesp);
        },
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

TVarInst *TypeResolver::get_var_local_global(std::string_view name) {
  for (auto tnamesp = cur_tnamesp; tnamesp != nullptr;
       tnamesp = tnamesp->parent) {
    if (auto it = tnamesp->variables.find(name);
        it != tnamesp->variables.end()) {
      return it->second;
    }
    if (tnamesp == cur_fun_tnamesp) {
      break; // reached function namespace
    }
  }
  if (auto it = root_tnamesp->variables.find(name);
      it != root_tnamesp->variables.end()) {
    return it->second;
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
      // clang-format off
      return std::visit(overload{
        [&](Primitive *) -> DeclaredName {
          // all primitive types should be added at the beginning of resolving
          return tnamesp->concrete_types[geninst_str];
        },
        [&](BuiltinType *decl) -> DeclaredName {
          if (auto it = tnamesp->concrete_types.find(geninst_str); it != tnamesp->concrete_types.end()) {
            return it->second;
          }
          return add_type(geninst, decl, tnamesp);
        },
        [&](EnumDecl *decl) -> DeclaredName {
          if (!tnamesp->concrete_types.contains(geninst_str)) {
            return add_type(geninst, decl, tnamesp);
          }
          return tnamesp->concrete_types[geninst_str];
        },
        [&](FunDecl *) -> DeclaredName {
          if (!tnamesp->concrete_funs.contains(geninst_str)) {
            return add_fun(geninst, std::get<FunDecl *>(decl), tnamesp);
          }
          return tnamesp->concrete_funs[geninst_str];
        },
        [&](StructDecl *decl) -> DeclaredName {
          if (!tnamesp->concrete_types.contains(geninst_str)) {
            return add_type(geninst, decl, tnamesp);
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
      }, decl);
      // clang-format on
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

  TypeId res = static_cast<int>(types.size());
  // clang-format off
  std::visit(overload{
    [&](Primitive *decl) {
      parent_tnamesp->concrete_types[type_name] = res;
      types.push_back(TTypeInst{*decl});
    },
    [&](BuiltinType *) {
      parent_tnamesp->concrete_types[type_name] = res;
      if (type.base_name == "Ref") {
        if (type.args.size() != 1) {
          error(fmt::format("Expected 1 arg for Ref[T], got {}.", type.args.size()));
        }
        types.push_back(TTypeInst{TBuiltinType{type, {}}});
        types[res].as<TBuiltinType>().args.emplace<Ref>(get_typeid(type.args[0]));
      }
      else if (type.base_name == "VarRef") {
        if (type.args.size() != 1) {
          error(fmt::format("Expected 1 arg for VarRef[T], got {}.", type.args.size()));
        }
        types.push_back(TTypeInst{TBuiltinType{type, {}}});
        types[res].as<TBuiltinType>().args.emplace<VarRef>(get_typeid(type.args[0]));
      }
      else if (type.base_name == "Span") {
        if (type.args.size() != 1) {
          error(fmt::format("Expected 1 arg for Span[T], got {}.", type.args.size()));
        }
        types.push_back(TTypeInst{TBuiltinType{type, {}}});
        types[res].as<TBuiltinType>().args.emplace<Span>(get_typeid(type.args[0]));
      }
      else if (type.base_name == "VarSpan") {
        if (type.args.size() != 1) {
          error(fmt::format("Expected 1 arg for VarSpan[T], got {}.", type.args.size()));
        }
        types.push_back(TTypeInst{TBuiltinType{type, {}}});
        types[res].as<TBuiltinType>().args.emplace<VarSpan>(get_typeid(type.args[0]));
      }
      else if (type.base_name == "Array") {
        if (type.args.size() != 2) {
          error(fmt::format(
              "Expected 2 args for Array[T, N], got {}.", type.args.size()
          ));
        }
        types.push_back(TTypeInst{TBuiltinType{type, {}}});
        try {
          types[res].as<TBuiltinType>().args.emplace<Array>(get_typeid(type.args[0]),
          stoll(type.args[1].to_string()));
        } catch (const std::invalid_argument &) {
          std::get<Array>(types[res].as<TBuiltinType>().args
          ) = {get_typeid(type.args[0]), std::stoll(type.args[1].to_string())};
          error(fmt::format(
              "Invalid size for Array[T, N]: {}", type.args[1].to_string()
          ));
        } catch (const std::out_of_range &) {
          error(fmt::format(
              "Size {} for Array[T, N] out of range", type.args[1].to_string()
          ));
        }
      }
      else {
        error(fmt::format("Unexpected builtin type {}.", type.base_name));
      }
    },
    [&](EnumDecl *decl) {
      types.push_back(TTypeInst{TEnumInst{type, {}, std::make_unique<TNamespace>(parent_tnamesp), {}, {}}});
      currently_creating.insert(type_name);
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
      currently_creating.erase(type_name);
    },
    [&](StructDecl *decl) {
      types.push_back(TTypeInst{TStructInst{type, {}, std::make_unique<TNamespace>(parent_tnamesp), {}, {}}});
      currently_creating.insert(type_name);
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
      currently_creating.erase(type_name);
    },
  }, decl);
  // clang-format on
  parent_tnamesp->concrete_types[type_name] = res;
  type_topo_order.push_back(res);
  return res;
}

FunId TypeResolver::add_fun(
    const GenericInst &fun, FunDecl *decl, TNamespace *parent_tnamesp
) {
  FunId res = static_cast<int>(functions.size());

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

  for (auto &param : decl->params) {
    functions[res].params.push_back(resolve(*param));
  }

  functions[res].return_type = get_typeid(decl->return_type);

  auto f = push_fun(&functions[res], block_namesp.get());
  functions[res].body = resolve(*decl->body, std::move(block_namesp));

  return res;
}

TypeId TypeResolver::find_unary_op(UnaryOp op, TypeId operand_type) {
  switch (op) {
  case UnaryOp::PLUS:
  case UnaryOp::NEG:
    if (!numeric_primitive_ids.contains(operand_type)) {
      error(fmt::format(
          "Invalid unary operation: {} {}\n", to_string(op), operand_type.value
      ));
    }
    return operand_type;
  case UnaryOp::NOT:
    if (operand_type != primitive_map.at("bool")) {
      error(fmt::format(
          "Invalid unary operation: {} {}\n", to_string(op), operand_type.value
      ));
    }
    return operand_type;
  case UnaryOp::REF:
    return get_typeid(GenericInst{
        "Ref", std::vector<GenericInst>{types[operand_type].concrete_type()}});
  case UnaryOp::DEREF: {
    const TTypeInst &type = types[operand_type];
    if (const TBuiltinType *bt = std::get_if<TBuiltinType>(&type.def);
        bt != nullptr && bt->args.index() == TBuiltinEnum::Ref) {
      return std::get<TBuiltinEnum::Ref>(bt->args);
    }
    error(fmt::format("Tried to deref a non-Ref type: {}", operand_type.value));
  }
  default:
    error(fmt::format(
        "Invalid unary operation: {} {}\n", to_string(op), operand_type.value
    ));
  }
}

TypeId
TypeResolver::find_binary_op(BinaryOp op, TypeId lhs_type, TypeId rhs_type) {
  if (numeric_primitive_ids.contains(lhs_type) &&
      numeric_primitive_ids.contains(rhs_type)) {
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
      return primitive_map.at("bool");
    default:;
    }
  } else if (lhs_type == primitive_map.at("bool") && rhs_type == primitive_map.at("bool")) {
    switch (op) {
    case BinaryOp::AND:
    case BinaryOp::OR:
    case BinaryOp::XOR:
      return primitive_map.at("bool");
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
    [&](std::monostate) -> std::optional<TStmt> { return std::nullopt; },
    [&](std::unique_ptr<Assign> &stmt) -> std::optional<TStmt> {
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
            stmt->lhs.s_expr(0, 2), types[res->lhs.type().value].name(), stmt->rhs.s_expr(0, 2), types[res->rhs.type().value].name()));
      }
      return TStmt{std::move(res)};
    },
    [&](std::unique_ptr<Break> &) -> std::optional<TStmt> { return TStmt{std::make_unique<TBreak>()}; },
    [&](std::unique_ptr<Continue> &) -> std::optional<TStmt> { return TStmt{std::make_unique<TContinue>()}; },
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
    [&](std::unique_ptr<For> &) -> std::optional<TStmt> { return std::nullopt; },
    [&](std::unique_ptr<Return> &stmt) -> std::optional<TStmt> {
      auto res = std::make_unique<TReturn>();
      if (stmt->value.has_value()) {
        res->value = resolve(stmt->value.value());
      }
      else {
        res->value = std::nullopt;
      }
      // expect no return expr
      if (cur_fun->return_type == primitive_map.at("unit")) {
        if (res->value.has_value() && res->value.value().type() != primitive_map.at("unit")) {
          error(primitive_map.at("unit"), res->value.value().type());
        }
      }
      // expect return expr matching function declaration
      else if (!res->value.has_value() || res->value.value().type() != cur_fun->return_type) {
        error(cur_fun->return_type, res->value.value().type());
      }
      return TStmt{std::move(res)};
    },
    [&](std::unique_ptr<While> &) -> std::optional<TStmt> { return std::nullopt; },
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
        [&](const Primitive &p) -> TypeId {
          error(fmt::format("Currently does not support dot ref of primitive type {}.", p.name.str));
        },
        [&](const TBuiltinType &bt) -> TypeId {
          error(fmt::format("Currently does not support dot ref of primitive type {}.", bt.concrete_type.base_name));
        },
        [&](const TEnumInst &inst) {
          if (auto it = inst.variants.find(res->name.str); it != inst.variants.end()) {
            res->prop_idx = it->second.second;
            return it->second.first;
          }
          if (auto it = inst.methods.find(res->name.str); it != inst.methods.end()) {
            return primitive_map.at("__fun");
          }
          error(fmt::format("Name {} in dot ref not found.", expr->name.str));
        },
        [&](const TStructInst &inst) {
          if (auto it = inst.fields.find(res->name.str); it != inst.fields.end()) {
            res->prop_idx = it->second.second;
            return it->second.first;
          }
          if (auto it = inst.methods.find(res->name.str); it != inst.methods.end()) {
            return primitive_map.at("__fun");
          }
          error(fmt::format("Name {} in dot ref not found.", expr->name.str));
        }
      }, t.def);
      
      return TExpr{std::move(res)};
    },
    [&](std::unique_ptr<FunCall> &expr) {
      auto res = std::make_unique<TFunCall>();
      res->callee = resolve(expr->callee);

      if (res->callee.type() != primitive_map["__fun"]) {
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
        const TDotRef &dot = *res->callee.as<std::unique_ptr<TDotRef>>();
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
      const auto &fun_inst = get_fun(res->fun);
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
        if (tbranch.condition.type() != primitive_map["bool"]) {
          error(primitive_map["bool"], tbranch.condition.type());
        }
        tbranch.block = resolve(*branch->block);
      }

      if (!res->has_else()) {
        res->type = primitive_map["unit"];
      }
      else {
        res->type = res->branches.front()->block->type;
        for (size_t i = 1; i < res->branches.size(); i++) {
          if (res->type != res->branches[i]->block->type) {
            res->type = primitive_map["unit"];
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
        [&](int32_t) { return primitive_map["i32"]; },
        [&](int64_t) { return primitive_map["i64"]; },
        [&](float)   { return primitive_map["f32"]; },
        [&](double)  { return primitive_map["f64"]; },
        [&](bool)    { return primitive_map["bool"]; },
        [&](char)    { return primitive_map["char"]; },
        [&](const std::string &) { return primitive_map["Span[char]"]; },
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
        res->type = primitive_map["__fun"];
      }
      else if (TVarInst **var_inst = std::get_if<TVarInst *>(&res->decl)) {
        TVarInst *correct_var = get_var_local_global(expr->name.str);
        if (*var_inst != correct_var) {
          error(fmt::format("Function referred to nonlocal variable which was not a global."));
        }
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
    res->type = primitive_map["unit"];
  }
  return res;
}

std::unique_ptr<TBlock> TypeResolver::resolve(Block &block) {
  auto tnamesp = std::make_unique<TNamespace>(cur_tnamesp);
  return resolve(block, std::move(tnamesp));
}

TAST TypeResolver::resolve() {
  functions.clear();
  types.clear();
  type_topo_order.clear();
  builtin_types.clear();
  primitive_map.clear();

  cur_ast_namesp = ast.globals.get();
  auto root_namesp = std::make_unique<TNamespace>(nullptr);
  root_tnamesp = root_namesp.get();
  cur_tnamesp = root_tnamesp;

  TAST tast;

  std::vector<std::unique_ptr<TVarInst>> globals;

  for (const auto &name : default_primitives) {
    primitive_map[name] = add_type(
        GenericInst{name}, std::get<Primitive *>(ast.globals->get_name(name)),
        root_tnamesp
    );
  }
  for (auto name : numeric_primitive_names) {
    numeric_primitive_ids.insert(primitive_map[name]);
  }

  for (Declaration &decl : ast.decls) {
    auto res = resolve(decl);
    if (res.has_value()) {
      globals.push_back(std::move(res.value()));
    }
  }

  tast.functions = std::move(functions);
  tast.types = std::move(types);
  tast.type_topo_order = std::move(type_topo_order);
  tast.globals = std::move(globals);
  tast.root_namesp = std::move(root_namesp);
  tast.primitive_map = std::move(primitive_map);

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
