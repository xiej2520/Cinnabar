#include "typecheck_resolver.hpp"

#include "fmt/core.h"

namespace cinnabar {

template <class... Ts> struct overload : Ts... { using Ts::operator()...; };
template <class... Ts> overload(Ts...) -> overload<Ts...>; // helloooo clang???

TypeResolver::TypeResolver(const AST &ast) : ast(ast) {}

std::vector<std::string> default_builtin_types = {
    "i8",         "i16", "i32",    "i64",   "u8",       "u16",
    "u32",        "u64", "f32",    "f64",   "char",     "bool",
    "unit",       "Ref", "VarRef", "Slice", "VarSlice",

    "Slice[char]" // String for now
};

TNamespace reserve_names(const Namespace &namesp) {
  TNamespace Tnamesp;
  for (auto &p : namesp.names) {
    switch (p.second.first) {
    case variant_index<DeclVariant, EnumDecl>():
      Tnamesp.reserve_type(p.first);
      break;
    case variant_index<DeclVariant, FunDecl>():
      Tnamesp.reserve_fun(p.first);
      break;
    case variant_index<DeclVariant, StructDecl>():
      Tnamesp.reserve_type(p.first);
      break;
    case variant_index<DeclVariant, VarDecl>():
      Tnamesp.reserve_var(p.first);
      break;
    default:
      abort();
    }
  }
  return Tnamesp;
}

TypeId TypeResolver::reserve_type() {
  TypeId id = tast_types->size();
  Type type{static_cast<TEnumDecl *>(nullptr), static_cast<int>(id)};
  tast_types->push_back(type);
  return id;
}

void TypeResolver::link_type(TypeId id, TTypeDeclPtr type_decl_ptr) {
  (*tast_types)[id].type_decl_ptr = type_decl_ptr;
}

TypeId TypeResolver::find_type(std::string_view name) {
  for (int i = namespaces.size() - 1; i >= 0; i--) {
    if (namespaces[i]->type_decls.contains(name)) {
      return namespaces[i]->type_decls[name];
    }
  }
  fmt::print(stderr, "Could not find type {}", name);
  abort();
}

TypeId TypeResolver::find_binary_op(BinaryOp op, TypeId lhs_type,
                                    TypeId rhs_type) {
  fmt::print(stderr, "Invalid binary operation: {} {} {}\n", to_string(op),
             lhs_type, rhs_type);
  abort();
}

// clang-format off
TStmt TypeResolver::resolve(const Stmt &stmt) {
  return std::visit(overload{
    [&](const std::monostate) { return TStmt{std::monostate{}}; },
    [&](const Assign &stmt) { return TStmt{std::make_unique<TAssign>(stmt.op, resolve(*stmt.lhs), resolve(*stmt.rhs))}; },
    [&](const Break &) { return TStmt{std::make_unique<TBreak>()}; },
    [&](const Continue &) { return TStmt{std::make_unique<TContinue>()}; },
    [&](const Declaration &stmt) { return TStmt{resolve(stmt)}; },
    [&](const Expression &stmt) { return TStmt{std::make_unique<TExpression>(resolve(stmt.expr))}; },
    [&](const For &) { return TStmt{std::monostate{}}; },
    [&](const Return &) { return TStmt{std::monostate{}}; },
    [&](const While &) { return TStmt{std::monostate{}}; },
  }, stmt.node);
}

TDeclaration TypeResolver::resolve(const Declaration &decl) {
  return std::visit(overload{
    [&](const EnumDecl &decl) {
      TNamespace namesp = reserve_names(decl.namesp);
      TypeId enum_id = reserve_type();
      // outer namespace
      namespaces.back()->link_type(decl.name.str, enum_id);
      namespaces.push_back(&namesp);
      // enum namespace
      namespaces.back()->link_type(decl.name.str, enum_id);

      std::vector<std::pair<Token, TypeId>> variants;
      for (auto &p : decl.variants) {
        variants.emplace_back(p.first, find_type(p.second.name));
      }

      std::vector<std::unique_ptr<TFunDecl>> methods;
      for (auto &fun : decl.methods) {
        methods.push_back(resolve(fun));
      }
      
      auto res = std::make_unique<TEnumDecl>(decl.name, variants, std::move(methods), namesp);
      link_type(enum_id, res.get());

      namespaces.pop_back();

      return TDeclaration{std::move(res)};
    },
    [&](const FunDecl &decl) {
      return TDeclaration{resolve(decl)};
    },
    [&](const StructDecl &decl) {
      TNamespace namesp = reserve_names(decl.namesp);
      TypeId struct_id = reserve_type();
      // outer namespace
      namespaces.back()->link_type(decl.name.str, struct_id);
      namespaces.push_back(&namesp);
      // enum namespace
      namespaces.back()->link_type(decl.name.str, struct_id);

      std::vector<std::pair<Token, TypeId>> fields;
      for (auto &p : decl.fields) {
        fields.emplace_back(p.first, find_type(p.second.name));
      }

      std::vector<std::unique_ptr<TFunDecl>> methods;
      for (auto &fun : decl.methods) {
        methods.push_back(resolve(fun));
      }
      
      auto res = std::make_unique<TStructDecl>(decl.name, fields, std::move(methods), namesp);
      link_type(struct_id, res.get());

      namespaces.pop_back();

      return TDeclaration{std::move(res)};
    },
    [&](const VarDecl &decl) {
      TExpr initializer = resolve(*decl.initializer);
      auto var_decl = std::make_unique<TVarDecl>(decl.name, initializer.type(), std::move(initializer));
      
      namespaces.back()->link_var(decl.name.str, var_decl.get());

      return TDeclaration{std::move(var_decl)};
    },
  }, decl.decl);
}

TExpr TypeResolver::resolve(const Expr &expr) {
  return std::visit(overload{
    [&](const Binary &expr) {
      auto lhs = resolve(*expr.left);
      auto rhs = resolve(*expr.right);

      TypeId id = find_binary_op(expr.op, lhs.type(), rhs.type());

      return TExpr{std::make_unique<TBinary>(id, expr.op, std::move(lhs), std::move(rhs))};
    },
    [&](const Block &expr) {
      return TExpr{resolve(expr)};
    },
    [&](const DotRef &) { return TExpr{std::make_unique<TLiteral>(find_type("i32"), 0)}; },
    [&](const FunCall &) { return TExpr{std::make_unique<TLiteral>(find_type("i32"), 0)}; },
    [&](const If &) { return TExpr{std::make_unique<TLiteral>(find_type("i32"), 0)}; },
    [&](const Literal &expr) {
      return TExpr{std::make_unique<TLiteral>(std::visit(overload{
        [&](i32) { return TLiteral{find_type("i32"), expr.val}; },
        [&](i64) { return TLiteral{find_type("i64"), expr.val}; },
        [&](f32) { return TLiteral{find_type("f32"), expr.val}; },
        [&](f64) { return TLiteral{find_type("f64"), expr.val}; },
        [&](bool) { return TLiteral{find_type("bool"), expr.val}; },
        [&](char) { return TLiteral{find_type("char"), expr.val}; },
        [&](std::string) { return TLiteral{find_type("Slice[char]"), expr.val}; },
      }, expr.val))};
    },
    [&](const Unary &) { return TExpr{std::make_unique<TLiteral>(find_type("i32"), 0)}; },
    [&](const Variable &) { return TExpr{std::make_unique<TLiteral>(find_type("i32"), 0)}; },
  }, expr.node);
}
// clang-format on

std::unique_ptr<TBlock> TypeResolver::resolve(const Block &block) {
  TNamespace namesp = reserve_names(block.namesp);
  namespaces.push_back(&namesp);

  std::vector<TStmt> stmts;
  for (auto &stmt : block.stmts) {
    stmts.push_back(resolve(stmt));
  }

  TypeId id = stmts.empty() || !stmts.back().is<std::unique_ptr<TExpression>>()
                  ? find_type("unit")
                  : stmts.back().as<std::unique_ptr<TExpression>>()->expr.type();
  namespaces.pop_back();
  return std::make_unique<TBlock>(id, std::move(stmts), namesp);
}

std::unique_ptr<TFunDecl> TypeResolver::resolve(const FunDecl &fun) {
  std::vector<std::pair<Token, TypeId>> params;
  auto res = std::make_unique<TFunDecl>(fun.name, params, resolve(fun.body));
  // link function in surrounding namespace
  namespaces.back()->link_fun(fun.name.str, res.get());
  // link function in function body
  res->body->namesp.link_fun(fun.name.str, res.get());

  return res;
}

TAST TypeResolver::resolve() {
  std::vector<TDeclaration> decls;

  TNamespace globals = reserve_names(ast.globals);
  namespaces.push_back(&globals);

  std::vector<std::unique_ptr<TBuiltin>> builtins;
  tast_types = std::make_unique<std::vector<Type>>();

  for (auto name : default_builtin_types) {
    if (ast.globals.names.contains(name)) {
      fmt::print(stderr, "Globals contained name '{}' matching builtin.\n",
                 name);
      abort();
    }
    builtins.emplace_back(std::make_unique<TBuiltin>(name));
    TypeId t = reserve_type();
    link_type(t, builtins.back().get());

    globals.link_type(builtins.back()->name, t);
  }

  for (const Declaration &decl : ast.decls) {
    decls.push_back(resolve(decl));
  }

  TAST tast(std::move(decls), std::move(builtins), std::move(*tast_types),
            globals);
  return tast;
}

} // namespace cinnabar
