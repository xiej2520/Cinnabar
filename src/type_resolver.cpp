#include "type_resolver.hpp"
#include "lexer.hpp"
#include "tast.hpp"

#include <functional>
#include <memory>
#include <ranges>
#include <string>
#include <variant>

namespace cinnabar {

TypeResolver::PushNamespace
TypeResolver::push_namespace(Namespace *namesp, TNamespace *tnamesp) {
  return {this, namesp, tnamesp};
}
TypeResolver::PushFun
TypeResolver::push_fun(FunctionInst *fun, TNamespace *fun_tnamesp) {
  return {this, fun, fun_tnamesp};
}

std::unique_ptr<TNamespace>
TypeResolver::make_tnamespace(TNamespace *parent_tnamesp) {
  return std::make_unique<TNamespace>(namespaces.size(), parent_tnamesp);
}

TypeResolver::TypeResolver(AST &ast) : ast(ast) {}

TGenericArg TypeResolver::resolve(const GenericArg &arg) {
  // clang-format off
  return std::visit(overload{
    [&](const GenericInst &inst) {
      return TGenericArg{get_type(resolve(inst))};
    },
    [&](const Literal &literal) {
      auto res = std::make_unique_for_overwrite<TVarInst>();
      res->name = Token::make_builtin("__GENERIC_ARG_LITERAL", Lexeme::IDENTIFIER);
      res->initializer = TExpr{resolve(literal)};
      res->type = res->initializer->type();

      return TGenericArg{std::move(res)};
    }
  }, arg.data);
  // clang-format on
}

TGenericInst TypeResolver::resolve(const GenericInst &inst) {
  TGenericInst res{};
  res.base_name = inst.base_name;
  for (auto &arg : inst.args) {
    res.args.push_back(resolve(arg));
  }
  return res;
}

std::unique_ptr<TLiteral> TypeResolver::resolve(const Literal &literal) {
  auto res = std::make_unique_for_overwrite<TLiteral>();
  // clang-format off
  res->type = std::visit(overload{
    [&](int32_t) { return builtin_types["i32"]; },
    [&](int64_t) { return builtin_types["i64"]; },
    [&](float)   { return builtin_types["f32"]; },
    [&](double)  { return builtin_types["f64"]; },
    [&](bool)    { return builtin_types["bool"]; },
    [&](char)    { return builtin_types["u8"]; },
    [&](const std::string &) { return builtin_types["Span[u8]"]; }
  }, literal.val);
  // clang-format on
  res->val = literal.val;
  return res;
}

TypeId TypeResolver::get_type(TGenericInst concrete) {
  auto namesp = cur_ast_namesp;
  auto tnamesp = cur_tnamesp;

  auto base_str = concrete.base_name.str;
  auto concrete_full_name =
      cinnabar::to_string(concrete.base_name, concrete.args, types);

  if (base_str == "Ref") {
    if (!root_tnamesp->items.contains(concrete_full_name)) {
      return std::get<TypeId>(
          root_tnamesp->items[concrete_full_name] =
              create_ref_type(std::move(concrete))
      );
    }
    return std::get<TypeId>(root_tnamesp->items[concrete_full_name]);
  } else if (base_str == "Array") {
    if (!root_tnamesp->items.contains(concrete_full_name)) {
      return std::get<TypeId>(
          root_tnamesp->items[concrete_full_name] =
              create_array_type(std::move(concrete))
      );
    }
    return std::get<TypeId>(root_tnamesp->items[concrete_full_name]);
  } else if (base_str == "Span") {
    if (!root_tnamesp->items.contains(concrete_full_name)) {
      return std::get<TypeId>(
          root_tnamesp->items[concrete_full_name] =
              create_span_type(std::move(concrete))
      );
    }
    return std::get<TypeId>(root_tnamesp->items[concrete_full_name]);
  }

  // clang-format off
  for (; namesp != nullptr; namesp = namesp->parent, tnamesp = tnamesp->parent) {
    if (auto it = namesp->names.find(base_str); it != namesp->names.end()) {
      /// add generic version of type to tast - later
      // if (!tnamesp->items.contains(base_str)) {
      //   tnamesp[base_str] = resolve(decl);
      // }

      return std::get<TypeId>(std::visit(overload{
        [&](BuiltinType) -> ItemRef {
          return builtin_types[base_str];
        },
        [&](EnumDecl *decl) -> ItemRef {
          if (!tnamesp->items.contains(concrete_full_name)) {
            return tnamesp->items[concrete_full_name] = create_enum_type(decl, std::move(concrete.args), tnamesp);
          }
          return tnamesp->items[concrete_full_name];
        },
        [&](FunDecl *) -> ItemRef { error(fmt::format("Name {} is a function, not a type.", base_str)); },
        [&](StructDecl *decl) -> ItemRef {
          if (!tnamesp->items.contains(concrete_full_name)) {
            return tnamesp->items[concrete_full_name] = create_struct_type(decl, std::move(concrete.args), tnamesp);
          }
          return tnamesp->items[concrete_full_name];
        },
        [&](VarDecl *) -> ItemRef { error(fmt::format("Name {} is a variable, not a type.", base_str)); },
      }, it->second));
    }
  }
  // clang-format on
  error(fmt::format("Base type {} not found.", base_str));
}

FunctionInst *TypeResolver::get_function(TGenericInst concrete) {
  auto namesp = cur_ast_namesp;
  auto tnamesp = cur_tnamesp;

  auto base_str = concrete.base_name.str;

  for (; namesp != nullptr;
       namesp = namesp->parent, tnamesp = tnamesp->parent) {
    if (auto it = namesp->names.find(base_str); it != namesp->names.end()) {
      if (!std::holds_alternative<FunDecl *>(it->second)) {
        error(fmt::format("Name {} is not a function.", base_str));
      }
      auto concrete_full_name =
          cinnabar::to_string(concrete.base_name, concrete.args, types);
      if (!tnamesp->items.contains(concrete_full_name)) {
        return std::get<FunctionInst *>(
            tnamesp->items[concrete_full_name] = create_function(
                std::get<FunDecl *>(it->second), std::move(concrete.args),
                tnamesp
            )
        );
      }
      return std::get<FunctionInst *>(tnamesp->items[concrete_full_name]);
    }
  }
  error(fmt::format("Function {} not found.", concrete.to_string(types)));
}

TVarInst *TypeResolver::get_var_local_global(std::string_view name) {
  std::string var_name{name};
  for (auto tnamesp = cur_tnamesp; tnamesp != nullptr;
       tnamesp = tnamesp->parent) {
    if (auto it = tnamesp->items.find(var_name); it != tnamesp->items.end()) {
      return std::get<TVarInst *>(it->second);
    }
    if (tnamesp == cur_fun_tnamesp) {
      break; // reached function namespace
    }
  }
  if (auto it = root_tnamesp->items.find(var_name);
      it != root_tnamesp->items.end()) {
    return std::get<TVarInst *>(it->second);
  }
  error(fmt::format("Variable {} not found.", name));
}

ItemRef TypeResolver::get_item_ref(const std::string &name) {
  bool past_function_namespace = false;
  for (auto tnamesp = cur_tnamesp; tnamesp != nullptr;
       tnamesp = tnamesp->parent) {
    if (auto it = tnamesp->items.find(name); it != tnamesp->items.end()) {
      if (past_function_namespaceT arInst&&
          std::holds_alternative<V *>(it->second)) {
        error(fmt::format("Name {} refers to a nonlocal variable.", name));
      }
      return it->second;
    }
    if (tnamesp == cur_fun_tnamesp) {
      past_function_namespace = true;
    }
  }
  error(fmt::format("Named value {} not found.", name));
}

TypeId TypeResolver::get_function_typeid(const FunctionType &ft) {
  std::string key{};
  for (auto arg_type : ft.arg_types) {
    key += arg_type;
    key += ' ';
  }
  key += to_string(ft.return_type);
  if (!function_types.contains(key)) {
    types.push_back(TypeRep{ft});
    return function_types[key] = types.size() - 1;
  }
  return function_types[key];
}

TypeId TypeResolver::create_struct_type(
    StructDecl *decl, std::vector<TGenericArg> args, TNamespace *parent_tnamesp
) {
  int struct_item_index = items.size();
  items.push_back(Item{std::make_unique<StructInst>()});
  auto struct_inst = std::get<std::unique_ptr<StructInst>>(items.back()).get();
  
  TypeId struct_type_index = types.size();
  types.push_back(TypeRep{Path{struct_inst, parent_tnamesp->id, struct_item_index}});
  struct_inst->generic_args = std::move(args);
  struct_inst->namesp = make_tnamespace(parent_tnamesp);

  currently_creating.insert(struct_type_index);

  auto p = push_namespace(decl->namesp.get(), struct_inst->namesp.get());

  auto concrete_full_name =
      cinnabar::to_string(decl->name_params.base_name, args, types);
  parent_tnamesp->items[concrete_full_name] = struct_type_index;
  struct_inst->namesp->items[concrete_full_name] = struct_type_index;

  if (args.size() != decl->name_params.params.size()) {
    error(fmt::format(
        "Expected {} generic arguments, received {}",
        decl->name_params.params.size(), args.size()
    ));
  }
  for (size_t i = 0; i < args.size(); i++) {
    // clang-format off
    struct_inst->namesp->items[std::string{decl->name_params.params[i].name.str}] =
      std::visit(overload{
        [&](TypeId t) { return ItemRef{t}; },
        [&](const std::unique_ptr<TVarInst> &v) { return ItemRef{v.get()}; }
      }, args[i].data);
    // clang-format on
  }

  for (size_t i = 0; i < decl->fields.size(); i++) {
    auto &[name, gen_type] = decl->fields[i];
    auto field_typeid = get_type(resolve(gen_type));
    if (Path *path = types[field_typeid].get_if<Path>()) {
      if (currently_creating.contains(path->item_index)) {
        error(fmt::format(
            "Cycle detected while instantiating types at {}.",
            gen_type.to_string()
        ));
      }
    }
    struct_inst->fields[name.str] = {field_typeid, i};
  }

  // add methods? later
  currently_creating.erase(struct_type_index);
  type_topo_order.push_back(struct_type_index);
  return struct_type_index;
}

TypeId TypeResolver::create_enum_type(
    EnumDecl *decl, std::vector<TGenericArg> args, TNamespace *parent_tnamesp
) {
  int enum_item_index = items.size();
  items.push_back(Item{std::make_unique<EnumInst>()});
  auto enum_inst = std::get<std::unique_ptr<EnumInst>>(items.back()).get();

  TypeId enum_type_index = types.size();
  types.push_back(TypeRep{Path{enum_inst, parent_tnamesp->id, enum_item_index}});
  enum_inst->generic_args = std::move(args);
  enum_inst->namesp = make_tnamespace(parent_tnamesp);

  currently_creating.insert(enum_type_index);

  auto p = push_namespace(decl->namesp.get(), enum_inst->namesp.get());

  auto concrete_full_name =
      cinnabar::to_string(decl->name_params.base_name, args, types);
  parent_tnamesp->items[concrete_full_name] = enum_type_index;
  enum_inst->namesp->items[concrete_full_name] = enum_type_index;

  for (size_t i = 0; i < args.size(); i++) {
    // clang-format off
    enum_inst->namesp->items[std::string{decl->name_params.params[i].name.str}] =
      std::visit(overload{
        [&](TypeId t) { return ItemRef{t}; },
        [&](const std::unique_ptr<TVarInst> &v) { return ItemRef{v.get()}; }
      }, args[i].data);
    // clang-format on
  }

  for (size_t i = 0; i < decl->variants.size(); i++) {
    auto &[name, gen_type] = decl->variants[i];
    auto variant_typeid = get_type(resolve(gen_type));
    if (Path *path = types[variant_typeid].get_if<Path>()) {
      if (currently_creating.contains(path->item_index)) {
        error(fmt::format(
            "Cycle detected while instantiating types at {}.",
            gen_type.to_string()
        ));
      }
    }
    enum_inst->variants[name.str] = {variant_typeid, i};
  }

  // add methods? later
  currently_creating.erase(enum_type_index);
  type_topo_order.push_back(enum_type_index);
  return enum_type_index;
}

TypeId TypeResolver::create_ref_type(TGenericInst concrete) {
  if (concrete.args.size() != 1 || !concrete.args[0].is<TypeId>()) {
    error(fmt::format(
        "Expected 1 type arg for Ref[_], got {}", concrete.to_string(types)
    ));
  }
  TypeId concrete_id = concrete.args[0].as<TypeId>();
  types.push_back(TypeRep{Ref{true, concrete_id}});
  return types.size() - 1;
}

TypeId TypeResolver::create_array_type(TGenericInst concrete) {
  if (concrete.args.size() != 2 || !concrete.args[0].is<TypeId>() ||
      !concrete.args[1].is<std::unique_ptr<TVarInst>>()) {
    error(fmt::format(
        "Expected 1 type arg and 1 size arg for Array[_, _], got {}",
        concrete.to_string(types)
    ));
  }
  auto var_inst_ptr = concrete.args[1].as<std::unique_ptr<TVarInst>>().get();
  size_t size;
  if (!var_inst_ptr->initializer.has_value()) {
    error("Array[_, _] size initializer is empty");
  }
  if (auto *literal_ptr =
          var_inst_ptr->initializer.value().get_node_if<TLiteral>()) {
    if (literal_ptr->type != builtin_types["i32"]) {
      error(fmt::format(
          "Array[_, _] size type is {}, not i32", to_string(literal_ptr->type)
      ));
    }
    size = std::get<int32_t>(literal_ptr->val);
  } else {
    error("Array[_, _] size initializer is not a literal");
  }

  TypeId concrete_id = concrete.args[0].as<TypeId>();
  types.push_back(TypeRep{Array{concrete_id, size}});
  return types.size() - 1;
}

TypeId TypeResolver::create_span_type(TGenericInst concrete) {
  if (concrete.args.size() != 1 || !concrete.args[0].is<TypeId>()) {
    error(fmt::format(
        "Expected 1 type arg for Span[_], got {}", concrete.to_string(types)
    ));
  }
  TypeId concrete_id = concrete.args[0].as<TypeId>();
  types.push_back(TypeRep{Span{true, concrete_id}});
  return types.size() - 1;
}

FunctionInst *TypeResolver::create_function(
    FunDecl *decl, std::vector<TGenericArg> args, TNamespace *parent_tnamesp
) {

  items.push_back(Item{std::make_unique<FunctionInst>()});
  auto fun_inst = std::get<std::unique_ptr<FunctionInst>>(items.back()).get();

  auto concrete_full_name =
      cinnabar::to_string(decl->name_params.base_name, args, types);

  auto block_namesp = make_tnamespace(parent_tnamesp);
  // add function name to allow recursion
  parent_tnamesp->items[concrete_full_name] = fun_inst;
  block_namesp->items[concrete_full_name] = fun_inst;

  if (args.size() != decl->name_params.params.size()) {
    error(fmt::format(
        "Expected {} generic arguments, received {}",
        decl->name_params.params.size(), args.size()
    ));
  }
  // add generic arguments to function namespace
  for (size_t i = 0; i < args.size(); i++) {
    // clang-format off
    block_namesp->items[std::string{decl->name_params.params[i].name.str}] =
      std::visit(overload{
        [&](TypeId t) { return ItemRef{t}; },
        [&](const std::unique_ptr<TVarInst> &v) { return ItemRef{v.get()}; }
      }, args[i].data);
    // clang-format on
  }

  auto p = push_namespace(decl->body->namesp.get(), block_namesp.get());

  FunctionType fun_type{};
  for (auto &param : decl->params) {
    fun_inst->params.push_back(resolve(*param));
    fun_type.arg_types.push_back(fun_inst->params.back()->type);
  }

  fun_inst->return_type = get_type(resolve(decl->return_type));
  fun_type.return_type = fun_inst->return_type;
  fun_inst->type = get_function_typeid(fun_type);

  auto f = push_fun(fun_inst, block_namesp.get());
  fun_inst->body = resolve(*decl->body, std::move(block_namesp));

  return fun_inst;
}

TGenericInst TypeResolver::index_to_tgeneric_inst(Index &index) {
  TGenericInst res{};
  NamedValue *named_val = index.callee.get_node_if<NamedValue>();
  if (named_val == nullptr) {
    error(fmt::format(
        "Unexpected error, called index_to_tgeneric_inst with non NamedValue "
        "callee {}",
        index.callee.node.index()
    ));
  }
  res.base_name = named_val->name;
  for (auto &arg : index.args) {
    if (NamedValue *name_arg = arg.get_node_if<NamedValue>()) {
      res.args.push_back(resolve(GenericArg{GenericInst{name_arg->name, {}}}));
    } else if (Index *index_arg = arg.get_node_if<Index>()) {
      res.args.push_back(TGenericArg{get_type(index_to_tgeneric_inst(*index_arg)
      )});
    } else if (Literal *literal_arg = arg.get_node_if<Literal>()) {
      res.args.push_back(resolve(GenericArg{*literal_arg}));
    }
  }
  return res;
}

std::unordered_set<Primitive> negatable = {PRIM_I8,  PRIM_I16,  PRIM_I32,
                                           PRIM_I64, PRIM_I128, PRIM_ISIZE,
                                           PRIM_F32, PRIM_F64};

TypeId TypeResolver::find_unary_op(UnaryOp op, const TExpr &operand) {
  auto operand_typerep = types[operand.type()];
  switch (op) {
  case UnaryOp::PLUS:
  case UnaryOp::NEG:
    if (!operand_typerep.is<Primitive>() ||
        !negatable.contains(operand_typerep.as<Primitive>())) {
      error(fmt::format(
          "Invalid unary operation: {} {}\n", cinnabar::to_string(op),
          operand_typerep.to_string(types)
      ));
    }
    return operand.type();
  case UnaryOp::NOT:
    if (!operand_typerep.is<Primitive>() ||
        operand_typerep.as<Primitive>() != PRIM_BOOL) {
      error(fmt::format(
          "Invalid unary operation: {} {}\n", cinnabar::to_string(op),
          operand_typerep.to_string(types)
      ));
    }
    return operand.type();
  case UnaryOp::REF: {
    if (!operand.is_place_expr()) {
      error("Operand of __ref is not a place expression.");
    }
    // initializer_list creates an array and copies it, must manually create
    // and move into args vector instead
    std::vector<TGenericArg> arg;
    arg.push_back(TGenericArg{operand.type()});
    return get_type(TGenericInst{
        Token::make_builtin("Ref", Lexeme::IDENTIFIER), std::move(arg)
    });
  }
  case UnaryOp::VARREF: {
    if (!operand.is_place_expr()) {
      error("Operand of __varref is not a place expression.");
    }
    std::vector<TGenericArg> arg;
    arg.push_back(TGenericArg{operand.type()});
    return get_type(TGenericInst{
        Token::make_builtin("VarRef", Lexeme::IDENTIFIER), std::move(arg)
    });
  }
  case UnaryOp::DEREF: {
    if (!operand_typerep.is<Ref>()) {
      error(fmt::format(
          "Tried to deref a non-Ref type: {}", operand_typerep.to_string(types)
      ));
    }
    return operand_typerep.as<Ref>().arg;
  }
  default:
    error(fmt::format(
        "Invalid unary operation: {} {}\n", cinnabar::to_string(op),
        operand_typerep.to_string(types)
    ));
  }
}

TypeId
TypeResolver::find_binary_op(BinaryOp op, TypeId lhs_type, TypeId rhs_type) {
  switch (op) {
  case BinaryOp::EQ:
    if (lhs_type == rhs_type) {
      return builtin_types["bool"];
    }
    break;
  case BinaryOp::NEQ:
    if (lhs_type == rhs_type) {
      return builtin_types["bool"];
    }
    break;
  default:;
  }

  auto lhs_typerep = types[lhs_type];
  auto rhs_typerep = types[rhs_type];
  if (lhs_typerep.is<Primitive>() && rhs_typerep.is<Primitive>()) {
    auto lhs_prim = lhs_typerep.as<Primitive>();
    auto rhs_prim = rhs_typerep.as<Primitive>();
    if (lhs_type != rhs_type) {
      error(lhs_type, rhs_type);
    }
    if (lhs_prim != PRIM_BOOL && rhs_prim != PRIM_BOOL) {
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
        return builtin_types["bool"];
      default:;
      }
    } else if (lhs_prim == PRIM_BOOL && rhs_prim == PRIM_BOOL) {
      switch (op) {
      case BinaryOp::AND:
      case BinaryOp::OR:
      case BinaryOp::XOR:
        return builtin_types["bool"];
      default:;
      }
    }
  }
  error(fmt::format(
      "Invalid binary operation: {} {} {}\n", cinnabar::to_string(op),
      to_string(lhs_type), to_string(rhs_type)
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
      if (res->lhs.is_place_expr()) {

      }
      else {
        error("Left side of assignment is not an assignee expression.");
      }

      if (res->lhs.type() != res->rhs.type()) {
        error(fmt::format("Left side {} has type {}, does not match right side {} type {}.",
            stmt->lhs.s_expr(0, 2),
            to_string(res->lhs.type()),
            stmt->rhs.s_expr(0, 2),
            to_string(res->rhs.type())
        ));
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
      if (cur_fun->return_type == builtin_types["Unit"]) {
        if (res->value.has_value() && res->value.value().type() != builtin_types["Unit"]) {
          error(builtin_types["Unit"], res->value.value().type());
        }
      }
      // expect return expr matching function declaration
      else if (!res->value.has_value() || res->value.value().type() != cur_fun->return_type) {
        error(cur_fun->return_type, res->value.value().type());
      }
      return TStmt{std::move(res)};
    },
    [&](std::unique_ptr<While> &) -> std::optional<TStmt> { return std::nullopt; },
    [&](std::unique_ptr<Print> &stmt) -> std::optional<TStmt> {
      auto res = std::make_unique<TPrint>();
      res->newline = stmt->newline;
      std::vector<TGenericArg> args;
      args.push_back(TGenericArg{builtin_types["u8"]});
      auto str_span_type = get_type(TGenericInst{
          Token::make_builtin("Span", Lexeme::IDENTIFIER), std::move(args)
      });

      for (auto &expr : stmt->args) {
        res->args.push_back(resolve(expr));
        auto arg_type = res->args.back().type();
        auto arg_typerep = types[arg_type];
        if (!arg_typerep.is<Primitive>() && arg_type != builtin_types["Span[u8]"]) {
          error(fmt::format("Unexpected type id in print: {}", to_string(arg_type)));
        }
      }
      if (res->args.empty() && !res->newline) {
        error("__print must have 1 or more arguments");
      }
      if (res->args.size() > 1) {
        if (res->args.front().type() == str_span_type) {
          // if more than one arg, format string must be a literal so it can be
          // type checked
          if (!res->args.front().is<std::unique_ptr<TLiteral>>()) {
            error("__print must have literal format string with more than one argument");
          }
          std::string_view fmt_str = std::get<std::string>(res->args.front().as<std::unique_ptr<TLiteral>>()->val);

          // validate format string (number of arguments)
          size_t arg_idx = 1; // index of format
          for (size_t i = 0; i < fmt_str.size(); i++) {
            if (fmt_str[i] == '{') {
              if (i + 1 >= fmt_str.size()) {
                error("Unclosed '{' in format string");
              }
              if (fmt_str[i+1] == '{') {
                i++;
              }
              else if (fmt_str[i+1] == '}') {
                arg_idx++;
                i++;
              }
            }
            else if (fmt_str[i] == '}') {
              if (i + 1 >= fmt_str.size() || fmt_str[i+1] != '}') {
                error("Unmatched '}' in format string");
              }
              i++;
            }
          }
          if (arg_idx != res->args.size()) {
            error(fmt::format("Expected {} format arguments, got {}", arg_idx - 1, res->args.size() - 1));
          }

        }
        else {
          // more than 1 arg, first arg isn't Span[u8]
          error(fmt::format(
            "__print with more than 1 argument must have Span[u8] as first argument, found {}",
            to_string(res->args.front().type())));
        }
      }
      
      
      return TStmt{std::move(res)};
    }
  }, stmt.node);
}

std::optional<std::unique_ptr<TVarInst>> TypeResolver::resolve(Declaration &decl) {
  return std::visit(overload{
    [&](std::unique_ptr<EnumDecl> &decl) -> std::optional<std::unique_ptr<TVarInst>> {
      if (decl->name_params.is_concrete()) {
        get_type(TGenericInst{decl->name_params.base_name, {}});
      }
      return std::nullopt;
    },
    [&](std::unique_ptr<FunDecl> &decl) -> std::optional<std::unique_ptr<TVarInst>> {
      if (decl->name_params.is_concrete()) {
        get_function(TGenericInst{decl->name_params.base_name, {}});
      }
      return std::nullopt;
    },
    [&](std::unique_ptr<StructDecl> &decl) -> std::optional<std::unique_ptr<TVarInst>> {
      if (decl->name_params.is_concrete()) {
        get_type(TGenericInst{decl->name_params.base_name, {}});
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
    auto specifier_type = get_type(resolve(decl.type_specifier.value()));
    if (res->initializer.has_value() && res->type != specifier_type) {
        error(fmt::format("Variable {} has declared type {}, does not match initializer type {}",
            res->name.str, to_string(specifier_type), to_string(res->type)));
    }
    res->type = specifier_type;
  }
  if (res->type == -1) {
    error(fmt::format("Variable {} does not have a declared type or initializer"
        " (currently do not support type inference).", res->name.str));
  }
  
  cur_tnamesp->items[std::string{res->name.str}] = res.get();
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

      auto left_type = res->left.type();
      if (Path *p = types[left_type].get_if<Path>()) {
        if (EnumInst **pe = p->get_if<EnumInst *>()) {
          EnumInst *inst = *pe;
          if (auto it = inst->variants.find(res->name.str); it != inst->variants.end()) {
            res->prop_idx = it->second.second;
            res->type = it->second.first;
          }
          else if (auto it = inst->methods.find(res->name.str); it != inst->methods.end()) {
            // todo: fix later, give FunctionInst a type
            res->type = -1;
          } else {
            error(fmt::format("Name {} in dot ref not found.", expr->name.str));
          }
        } else if (StructInst **se = p->get_if<StructInst *>()) {
          StructInst *inst = *se;
          if (auto it = inst->fields.find(res->name.str); it != inst->fields.end()) {
            res->prop_idx = it->second.second;
            res->type = it->second.first;
          }
          else if (auto it = inst->methods.find(res->name.str); it != inst->methods.end()) {
            // todo: fix later, give FunctionInst a type
            res->type = -1;
          } else {
            error(fmt::format("Name {} in dot ref not found.", expr->name.str));
          }
        } else {
          error(fmt::format("Unexpected dot ref on {} path", p->to_string(types)));
        }
      } else {
        error(fmt::format("Unexpected dot ref on {} type", to_string(left_type)));
      }
      
      return TExpr{std::move(res)};
    },
    [&](std::unique_ptr<FunCall> &expr) {
      auto res = std::make_unique<TFunCall>();
      res->callee = resolve(expr->callee);

      if (!types[res->callee.type()].is<FunctionType>()) {
        error("Type of callee was not a function.");
      }

      // functions and struct constructors callable
      // find the function being called
      if (TFunctionName *fun_name = res->callee.get_node_if<TFunctionName>()) {
        res->type = fun_name->fun->return_type;
        res->fun = fun_name->fun;
      }
      // dotref to function callable (method)
      else if (TDotRef *dotref = res->callee.get_node_if<TDotRef>()) {
        res->fun = dotref->fun;
        res->type = dotref->fun->return_type;
      }
      else {
        error("Callee is not callable.");
      }
      
      auto *fun_inst = res->fun;
      // check arity
      if (fun_inst->params.size() != expr->args.size()) {
        error("Function declaration has different arity than function call.");
      }
      // check function type match
      for (size_t i=0; i < expr->args.size(); i++) {
        res->args.push_back(resolve(expr->args[i]));

        if (res->args[i].type() != fun_inst->params[i]->type) {
          error(fun_inst->params[i]->type, res->args[i].type());
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
        if (!types[tbranch.condition.type()].is<Primitive>() || types[tbranch.condition.type()].as<Primitive>() != PRIM_BOOL) {
          error(builtin_types["bool"], tbranch.condition.type());
        }
        tbranch.block = resolve(*branch->block);
      }

      if (!res->has_else()) {
        res->type = builtin_types["Unit"];
      }
      else {
        res->type = res->branches.front()->block->type;
        for (size_t i = 1; i < res->branches.size(); i++) {
          if (res->type != res->branches[i]->block->type) {
            res->type = builtin_types["Unit"];
            break;
          }
        }
      }

      return TExpr{std::move(res)};
    },
    [&](std::unique_ptr<Index> &expr) {
      if (NamedValue *named_val = expr->callee.get_node_if<NamedValue>()) {
        auto item = get_item_ref(std::string{named_val->name.str});

        if (TypeId *t = std::get_if<TypeId>(&item)) {
          auto tgeneric_inst = index_to_tgeneric_inst(*expr);

          return TExpr{std::make_unique<TTypeName>(get_type(std::move(tgeneric_inst)))};

        } else if (FunctionInst **fi = std::get_if<FunctionInst *>(&item)) {
          auto tgeneric_inst = index_to_tgeneric_inst(*expr);
          auto fun_inst = get_function(std::move(tgeneric_inst));

          return TExpr{std::make_unique<TFunctionName>(fun_inst->type, fun_inst)};

        }
      }

      // is an index operation
      auto res = std::make_unique<TIndex>();
      if (expr->args.size() != 1) {
        error("Currently only support indexing with 1 argument");
      }
      res->callee = resolve(expr->callee);
      res->arg = resolve(expr->args[0]);

      if (Array *array_type = types[res->callee.type()].get_if<Array>()) {
        res->type = array_type->arg;
      } else if (Span *span_type = types[res->callee.type()].get_if<Span>()) {
        res->type = span_type->arg;
      } else {
          error(fmt::format("Unsupported indexing operation on type {} with operand type {}",
              to_string(res->callee.type()), to_string(res->arg.type())));
      }

      return TExpr{std::move(res)};
    },
    [&](std::unique_ptr<Literal> &expr) {
      return TExpr{resolve(*expr)};
    },
    [&](std::unique_ptr<Unary> &expr) {
      auto res = std::make_unique_for_overwrite<TUnary>();
      res->op = expr->op;
      res->operand = resolve(expr->operand);
      
      res->type = find_unary_op(expr->op, res->operand);
      return TExpr{std::move(res)};
    },
    [&](std::unique_ptr<NamedValue> &expr) {
      // NamedValue is identifier, can be var, fun, struct, enum
      
      // order of resolution: find the closest name, can be type, function, or
      // variable. If type or function, search is done. If variable, check if
      // the found variable is either local to the function, or a global. If not,
      // it is invalid code, throw error

      auto item = get_item_ref(std::string{expr->name.str});
      return std::visit(overload {
        [&](const TypeId &type) -> TExpr {
          return TExpr{std::make_unique<TTypeName>(type)};
        },
        [&](FunctionInst *fun) -> TExpr {
          auto res = std::make_unique_for_overwrite<TFunctionName>();
          res->type = fun->type;
          res->fun = fun;
          return TExpr{std::move(res)};
        },
        [&](TVarInst *var) -> TExpr {
          auto res = std::make_unique_for_overwrite<TVariable>();
          TVarInst *correct_var = get_var_local_global(expr->name.str);
          if (var != correct_var) {
            error(fmt::format("Function referred to nonlocal variable which was not a global."));
          }
          res->name = expr->name;
          res->type = var->type;
          return TExpr{std::move(res)};
        },
      }, item);
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
    res->type = builtin_types["Unit"];
  }
  return res;
}

std::unique_ptr<TBlock> TypeResolver::resolve(Block &block) {
  auto tnamesp = make_tnamespace(cur_tnamesp);
  return resolve(block, std::move(tnamesp));
}

TAST TypeResolver::resolve() {
  items.clear();
  types.clear();
  currently_creating.clear();
  type_topo_order.clear();

  cur_ast_namesp = ast.globals.get();
  auto root_namesp = make_tnamespace(nullptr);
  root_tnamesp = root_namesp.get();
  cur_tnamesp = root_tnamesp;

  TAST tast{};

  std::vector<std::unique_ptr<TVarInst>> globals;

  builtin_types["Unit"] = 0;
  types.push_back(TypeRep{Unit{}});

  for (auto p : primitive_types) {
    root_tnamesp->items[name(p)] = static_cast<TypeId>(types.size());
    builtin_types[name(p)] = types.size();
    types.push_back(TypeRep{p});
  }
  std::vector<TGenericArg> u8_arg;
  u8_arg.push_back(TGenericArg{builtin_types["u8"]});
  builtin_types["Span[u8]"] = create_span_type(TGenericInst{
      Token::make_builtin("Span", Lexeme::IDENTIFIER), std::move(u8_arg)
  });

  for (auto &decl : ast.decls) {
    auto res = resolve(decl);
    if (res.has_value()) {
      globals.push_back(std::move(res.value()));
    }
  }

  tast.items = std::move(items);
  tast.types = std::move(types);
  tast.type_topo_order = std::move(type_topo_order);
  tast.root_namesp = std::move(root_namesp);

  return tast;
}

[[nodiscard]] std::string TypeResolver::to_string(TypeId id) const {
  return types[id].to_string(types);
}

[[noreturn]] void TypeResolver::error(std::string_view message) {
  fmt::print(stderr, "Type Resolution Error: {}\n", message);
  abort();
}

[[noreturn]] void TypeResolver::error(TypeId expected, TypeId found) {
  error(fmt::format(
      "Expected type {}, found type {}.", types[expected].to_string(types),
      types[found].to_string(types)
  ));
}

} // namespace cinnabar
