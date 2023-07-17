#include "tast.hpp"
#include "fmt/core.h"

namespace cinnabar {

using std::pair;
using std::unique_ptr;
using std::vector;

template <class... Ts> struct overload : Ts... { using Ts::operator()...; };
template <class... Ts> overload(Ts...) -> overload<Ts...>; // helloooo clang???

std::string_view Type::name() {
  return std::visit(
      overload{
          [](TBuiltin *builtin) { return std::string_view(builtin->name); },
          [](TEnumDecl *enum_decl) { return enum_decl->name.str; },
          [](TStructDecl *struct_decl) { return struct_decl->name.str; }},
      type_decl_ptr);
}

void TNamespace::link_type(std::string_view name, TypeId id) {
  type_decls[name] = id;
}
void TNamespace::link_fun(std::string_view name, TFunDecl *decl) {
  fun_decl_ptrs[name] = decl;
}
void TNamespace::link_var(std::string_view name, TVarDecl *decl) {
  var_decl_ptrs[name] = decl;
}
void TNamespace::reserve_type(std::string_view name) {
  type_decls[name] = -1;
}
void TNamespace::reserve_fun(std::string_view name) {
  fun_decl_ptrs[name] = nullptr;
}
void TNamespace::reserve_var(std::string_view name) {
  var_decl_ptrs[name] = nullptr;
}

TBuiltin::TBuiltin(std::string name) : name(name) {}

TAssign::TAssign(AssignOp op, TExpr lhs, TExpr rhs)
    : op(op), lhs(std::move(lhs)), rhs(std::move(rhs)) {}

TDeclaration::TDeclaration(TDeclVariant decl) : decl(std::move(decl)) {}

// declaration statements
TEnumDecl::TEnumDecl(Token name, vector<pair<Token, TypeId>> variants,
                     vector<unique_ptr<TFunDecl>> methods, TNamespace namesp)
    : name(name), variants(variants), methods(std::move(methods)),
      namesp(namesp) {}

TFunDecl::TFunDecl(Token name, vector<pair<Token, TypeId>> params,
                   unique_ptr<TBlock> body)
    : name(name), params(params), body(std::move(body)) {}

TStructDecl::TStructDecl(Token name, vector<pair<Token, TypeId>> fields,
                         vector<unique_ptr<TFunDecl>> methods,
                         TNamespace namesp)
    : name(name), fields(fields), methods(std::move(methods)), namesp(namesp) {}

TVarDecl::TVarDecl(Token name, TypeId type, TExpr initializer)
    : name(name), type(type), initializer(std::move(initializer)) {}

// expressions
TExpression::TExpression(TExpr expr) : expr(std::move(expr)) {}

TBinary::TBinary(TypeId type, BinaryOp op, TExpr lhs, TExpr rhs)
    : type(type), op(op), lhs(std::move(lhs)), rhs(std::move(rhs)) {}

TBlock::TBlock(TypeId type, vector<TStmt> stmts, TNamespace namesp)
    : type(type), stmts(std::move(stmts)), namesp(namesp) {}

TDotRef::TDotRef(TExpr lvalue, Token name)
    : lvalue(std::move(lvalue)), name(name) {}

TFunCall::TFunCall(TExpr callee, vector<TExpr> args)
    : callee(std::move(callee)), args(std::move(args)) {}

TIf::TIf(vector<unique_ptr<TBranch>> branches)
    : branches(std::move(branches)) {}

TIf::TBranch::TBranch(TExpr condition, TBlock block)
    : condition(std::move(condition)), block(std::move(block)) {}

TLiteral::TLiteral(TypeId type, LiteralVariant val) : type(type), val(val) {}

TVariable::TVariable(Token name) : name(name) {}

TUnary::TUnary(UnaryOp op, TExpr rhs) : op(op), rhs(std::move(rhs)) {}

TExpr::TExpr(TExprVariant node) : node(std::move(node)) {}

TypeId TExpr::type() {
  return std::visit(overload{
    [&](unique_ptr<TBinary> &expr) { return expr->type; },
    [&](unique_ptr<TBlock> &expr) { return expr->type; },
    [&](unique_ptr<TDotRef> &expr) { return expr->type; },
    [&](unique_ptr<TFunCall> &expr) { return expr->type; },
    [&](unique_ptr<TIf> &expr) { return expr->type; },
    [&](unique_ptr<TLiteral> &expr) { return expr->type; },
    [&](unique_ptr<TUnary> &expr) { return expr->type; },
    [&](unique_ptr<TVariable> &expr) { return expr->type; },
  }, node);
}

TAST::TAST(vector<TDeclaration> decls,
           vector<unique_ptr<TBuiltin>> builtins, vector<Type> types, TNamespace globals)
    : decls(std::move(decls)), builtin_types(std::move(builtins)),
      types(std::move(types)), globals(std::move(globals)) {}

std::string to_string(const TNamespace &namesp, int cur, int ind) {
  std::string res = fmt::format("{:{}}Namespace\n", "", cur);
  if (!namesp.type_decls.empty()) {
    res += fmt::format("{:{}}Types: ", "", cur + ind);
    for (auto &p : namesp.type_decls) {
      res += fmt::format("{}, ", p.first);
    }
    res.pop_back();
    res.back() = '\n';
  }
  if (!namesp.fun_decl_ptrs.empty()) {
    res += fmt::format("{:{}}Functions: ", "", cur + ind);
    for (auto &p : namesp.fun_decl_ptrs) {
      res += fmt::format("{}, ", p.first);
    }
    res.pop_back();
    res.back() = '\n';
  }
  if (!namesp.var_decl_ptrs.empty()) {
    res += fmt::format("{:{}}Variables: ", "", cur + ind);
    for (auto &p : namesp.var_decl_ptrs) {
      res += fmt::format("{}, ", p.first);
    }
    res.pop_back();
    res.back() = '\n';
  }
  return res;
}

std::string TAST::to_string() {
  std::string res("Builtin Types: ");
  for (auto &builtin : builtin_types) {
    res += builtin->name + " ";
  }
  res.back() = '\n';
  res += cinnabar::to_string(globals, 0, 2);
  return res;
}

} // namespace cinnabar