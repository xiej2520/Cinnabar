#pragma once

#include "ast_ops.hpp"

#include <memory>
#include <unordered_map>
#include <variant>
#include <vector>

namespace cinnabar {

struct TStmt;
struct TAssign;
struct TBreak;
struct TContinue;
struct TDeclaration;
struct TExpression;
struct TFor;
struct TReturn;
struct TWhile;

struct TEnumDecl;
struct TFunDecl;
struct TStructDecl;
struct TVarDecl;

struct TExpr;
struct TBinary;
struct TBlock;
struct TDotRef;
struct TFunCall;
struct TIf;
struct TLiteral;
struct TVariable;
struct TUnary;

using TDeclVariant =
    std::variant<std::unique_ptr<TEnumDecl>, std::unique_ptr<TFunDecl>,
                 std::unique_ptr<TStructDecl>, std::unique_ptr<TVarDecl>>;

struct TBuiltin {
  std::string name;
  TBuiltin(std::string name);
};

using TDeclPtr = std::variant<TBuiltin *, TEnumDecl *, TFunDecl *,
                              TStructDecl *, TVarDecl *>;
using TTypeDeclPtr = std::variant<TBuiltin *, TEnumDecl *, TStructDecl *>;

using TypeId = int;

struct Type {
  TTypeDeclPtr type_decl_ptr;
  int type_id;
  template <typename T> bool is() {
    return std::holds_alternative<T>(type_decl_ptr);
  }
  template <typename T> T &as() { return std::get<T>(type_decl_ptr); }
  std::string_view name();
};

struct TNamespace {
  std::unordered_map<std::string_view, TypeId> type_decls;
  std::unordered_map<std::string_view, TFunDecl *> fun_decl_ptrs;
  std::unordered_map<std::string_view, TVarDecl *> var_decl_ptrs;
  void reserve_type(std::string_view name);
  void reserve_fun(std::string_view name);
  void reserve_var(std::string_view name);
  void link_type(std::string_view name, TypeId id);
  void link_fun(std::string_view name, TFunDecl *decl);
  void link_var(std::string_view name, TVarDecl *decl);

  TNamespace() = default;
};

using TExprVariant =
    std::variant<std::unique_ptr<TBinary>, std::unique_ptr<TBlock>,
                 std::unique_ptr<TDotRef>, std::unique_ptr<TFunCall>,
                 std::unique_ptr<TIf>, std::unique_ptr<TLiteral>,
                 std::unique_ptr<TUnary>, std::unique_ptr<TVariable>>;

struct TExpr {
  TExprVariant node;
  TExpr(TExprVariant node);
  TypeId type();
  std::string s_expr(int cur, int ind); // current indent, indent
};

struct TBinary {
  TypeId type;
  BinaryOp op;
  TExpr lhs;
  TExpr rhs;
  TBinary(TypeId type, BinaryOp op, TExpr lhs, TExpr rhs);
};

struct TBlock {
  TypeId type;
  std::vector<TStmt> stmts;
  TNamespace namesp;
  TBlock(TypeId type, std::vector<TStmt> stmts, TNamespace namesp);
};

struct TDotRef {
  TypeId type;
  TExpr lvalue;
  Token name;
  TDotRef(TExpr lvalue, Token name);
};

struct TFunCall {
  TypeId type;
  TExpr callee;
  std::vector<TExpr> args;
  TFunCall(TExpr callee, std::vector<TExpr> args);
};

struct TIf {
  TypeId type;
  struct TBranch {
    TExpr condition;
    TBlock block;
    TBranch(TExpr condition, TBlock block);
  };
  std::vector<std::unique_ptr<TBranch>> branches;
  TIf(std::vector<std::unique_ptr<TBranch>> branches);
};

using LiteralVariant =
    std::variant<i32, i64, f32, f64, bool, char, std::string>;

struct TLiteral {
  TypeId type;
  LiteralVariant val;
  TLiteral(TypeId type, LiteralVariant val);
};

struct TVariable {
  TypeId type;
  Token name;
  TVariable(Token name);
};

struct TUnary {
  TypeId type;
  UnaryOp op;
  TExpr rhs;
  TUnary(UnaryOp op, TExpr rhs);
};

//
// statements
//
struct TAssign {
  AssignOp op;
  TExpr lhs;
  TExpr rhs;
  TAssign(AssignOp op, TExpr lhs, TExpr rhs);
};

struct TBreak {};
struct TContinue {};

struct TEnumDecl {
  Token name;
  std::vector<std::pair<Token, TypeId>> variants;
  std::vector<std::unique_ptr<TFunDecl>> methods;
  TNamespace namesp;
  TEnumDecl(Token name, std::vector<std::pair<Token, TypeId>> variants,
            std::vector<std::unique_ptr<TFunDecl>> methods, TNamespace namesp);
};

struct TFunDecl {
  Token name;
  std::vector<std::pair<Token, TypeId>> params;
  std::unique_ptr<TBlock> body;
  TFunDecl(Token name, std::vector<std::pair<Token, TypeId>> params,
           std::unique_ptr<TBlock> body);
  std::string s_expr(int cur, int ind);
};

struct TStructDecl {
  Token name;
  std::vector<std::pair<Token, TypeId>> fields;
  std::vector<std::unique_ptr<TFunDecl>> methods;
  TNamespace namesp;
  TStructDecl(Token name, std::vector<std::pair<Token, TypeId>> fields,
              std::vector<std::unique_ptr<TFunDecl>> methods,
              TNamespace namesp);
};

struct TVarDecl {
  Token name;
  TypeId type;
  TExpr initializer;
  TVarDecl(Token name, TypeId type, TExpr initializer);
};

struct TDeclaration {
  TDeclVariant decl;
  TDeclaration(TDeclVariant decl);
  TDeclaration(TDeclaration &&) = default;
  template <typename T> bool is() { return std::holds_alternative<T>(decl); }
  template <typename T> T &as() { return std::get<T>(decl); }
  std::string s_expr(int cur, int ind);
};

struct TExpression {
  TExpr expr;
  TExpression(TExpr expr);
};

struct TFor {};
struct TReturn {};
struct TWhile {};

using TStmtVariant = std::variant<
    std::monostate, std::unique_ptr<TAssign>, std::unique_ptr<TBreak>,
    std::unique_ptr<TContinue>, TDeclaration, std::unique_ptr<TExpression>,
    std::unique_ptr<TFor>, std::unique_ptr<TReturn>, std::unique_ptr<TWhile>>;

struct TStmt {
  TStmtVariant node;
  template <typename T> bool is() { return std::holds_alternative<T>(node); }
  template <typename T> T &as() { return std::get<T>(node); }
  std::string s_expr(int cur, int ind);
};

struct TAST {
  std::vector<TDeclaration> decls;

  std::vector<std::unique_ptr<TBuiltin>> builtin_types;
  std::vector<Type> types;

  TNamespace globals;

  TAST(std::vector<TDeclaration> decls,
       std::vector<std::unique_ptr<TBuiltin>> builtins, std::vector<Type> types,
       TNamespace globals);

  std::string to_string();
};

} // namespace cinnabar
