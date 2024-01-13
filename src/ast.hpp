#pragma once

#include "ast.hpp"

#include "ast_ops.hpp"
#include "common.hpp"
#include "lexer.hpp"

#include <memory>
#include <optional>
#include <unordered_map>
#include <variant>
#include <vector>

namespace cinnabar {

// statements
struct Stmt;
struct Assign;
struct Break;
struct Continue;
struct Declaration;
struct Expression;
struct For;
struct Return;
struct While;

// don't want to implement variadics or macros yet
struct Print;
// declaration statements
struct EnumDecl;
struct FunDecl;
struct StructDecl;
struct VarDecl;

// expressions
struct Expr;
struct Binary;
struct Block;
struct DotRef;
struct FunCall;
struct If;
struct Index;
struct Literal;
struct NamedValue;
struct Unary;

// extern const std::vector<std::string_view> default_primitives;

using DeclVariant = std::variant<
    std::unique_ptr<EnumDecl>, std::unique_ptr<FunDecl>,
    std::unique_ptr<StructDecl>, std::unique_ptr<VarDecl>>;

using DeclPtr = std::variant<EnumDecl *, FunDecl *, StructDecl *, VarDecl *>;
using TypeDeclPtr = std::variant<EnumDecl *, StructDecl *>;

using LiteralVariant =
    std::variant<int32_t, int64_t, float, double, bool, char, std::string>;
std::string to_string(LiteralVariant v);

struct Literal {
  LiteralVariant val;
  Literal(LiteralVariant val);
};

struct GenericArg;
// Array[Array[i32, 4], 8]
// needs to be reworked at some point, especially for const generics
struct GenericInst {
  Token base_name;
  std::vector<GenericArg> args;

  GenericInst(Token base_name);
  GenericInst(Token base_name, std::vector<GenericArg> args);

  [[nodiscard]] std::string to_string() const;
  [[nodiscard]] bool is_concrete() const;
};

struct GenericArg {
  std::variant<GenericInst, Literal> data;
  [[nodiscard]] std::string to_string() const;
};

struct GenericParam;
// Something like Array[T, N i32]
// or Foo[A Array[i32, 4], T]
struct GenericSignature {
  Token base_name;
  std::vector<GenericParam> params;
  [[nodiscard]] bool is_concrete() const;
  [[nodiscard]] std::string to_string() const;
};

struct TypeParamData {};
struct ValueParamData {
  GenericSignature type;
};
struct GenericParam {
  Token name;
  std::variant<TypeParamData, ValueParamData> param;
  [[nodiscard]] std::string to_string() const;
};

struct Namespace {
  std::unordered_map<std::string_view, DeclPtr> names; // base name -> decl

  Namespace *parent;

  Namespace(Namespace *parent);

  DeclPtr get_name(std::string_view name);
  std::string to_string(int cur);
};

/*
template <typename VariantType, typename T, std::size_t index = 0>
constexpr std::size_t variant_index() {
  static_assert(std::variant_size_v<VariantType> > index,
                "Type not found in variant");
  if constexpr (index == std::variant_size_v<VariantType>) {
    return index;
  } else if constexpr (std::is_same_v<
                           std::variant_alternative_t<index, VariantType>, T>) {
    return index;
  } else {
    return variant_index<VariantType, T, index + 1>();
  }
}
*/

//
// expressions
//

using ExprVariant = std::variant<
    std::unique_ptr<Binary>, std::unique_ptr<Block>, std::unique_ptr<DotRef>,
    std::unique_ptr<FunCall>, std::unique_ptr<If>, std::unique_ptr<Index>,
    std::unique_ptr<Literal>, std::unique_ptr<Unary>,
    std::unique_ptr<NamedValue>>;

struct Expr {
  ExprVariant node;
  Expr(ExprVariant node);

  // clang-format off
  template <typename T>
  bool is() { return std::holds_alternative<T>(node); }
  template <typename T>
  T &as() { return std::get<T>(node); }
  template <typename T>
  T *get_node_if() { return std::get_if<std::unique_ptr<T>>(&node)->get(); }
  // clang-format on
  std::string s_expr(int cur, int ind); // current indent, indent
};

struct Binary {
  BinaryOp op;
  Expr left;
  Expr right;
  Binary(BinaryOp op, Expr left, Expr right);
};

struct Block {
  std::vector<Stmt> stmts;
  std::unique_ptr<Namespace> namesp;
  Block(std::vector<Stmt> stmts, std::unique_ptr<Namespace> namesp);
  std::string to_string(int cur, int ind);
};

struct DotRef {
  Expr lvalue;
  // var.f()
  FunDecl *fun_decl = nullptr; // change to NamedValue?
  Token name;
  DotRef(Expr lvalue, Token name);
};

struct FunCall {
  Expr callee;
  std::vector<Expr> args;
  FunCall(Expr callee, std::vector<Expr> args);
};

struct If {
  struct Branch {
    // else branch will be a Literal true condition as the last block
    Expr condition;
    std::unique_ptr<Block> block;
    Branch(Expr condition, std::unique_ptr<Block> block);
  };
  std::vector<std::unique_ptr<Branch>> branches;
  If(std::vector<std::unique_ptr<Branch>> branches);
};

struct Index {
  Expr callee;
  // must be nonempty
  std::vector<Expr> args;
};

struct NamedValue {
  Token name; // replace with 'Path' construct later
  NamedValue(Token name);
};

struct Unary {
  UnaryOp op;
  Expr operand;
  Unary(UnaryOp op, Expr operand);
};

//
// statements
//
struct Assign {
  AssignOp op;
  Expr lhs;
  Expr rhs;
  Assign(AssignOp op, Expr lhs, Expr rhs);
};

struct Break {};
struct Continue {};

struct EnumDecl {
  GenericSignature name_params;
  std::vector<std::pair<Token, GenericInst>> variants; // unit type for empty
  std::vector<std::unique_ptr<FunDecl>> methods;
  std::unique_ptr<Namespace> namesp;
  EnumDecl(
      GenericSignature name_params,
      std::vector<std::pair<Token, GenericInst>> variants,
      std::vector<std::unique_ptr<FunDecl>> methods,
      std::unique_ptr<Namespace> namesp
  );
};

struct FunDecl {
  GenericSignature name_params;
  std::vector<std::unique_ptr<VarDecl>> params;
  GenericInst return_type;
  std::unique_ptr<Block> body;
  FunDecl(
      GenericSignature name_params,
      std::vector<std::unique_ptr<VarDecl>> params, GenericInst return_type,
      std::unique_ptr<Block> body
  );
  std::string s_expr(int cur, int ind);
};

struct StructDecl {
  GenericSignature name_params;
  std::vector<std::pair<Token, GenericInst>> fields;
  std::vector<std::unique_ptr<FunDecl>> methods;
  std::unique_ptr<Namespace> namesp;
  StructDecl(
      GenericSignature name_params,
      std::vector<std::pair<Token, GenericInst>> fields,
      std::vector<std::unique_ptr<FunDecl>> methods,
      std::unique_ptr<Namespace> namesp
  );
};

struct VarDecl {
  Token name;
  std::optional<GenericInst> type_specifier;
  std::optional<Expr> initializer;
  VarDecl(
      Token name, std::optional<GenericInst> type_specifier,
      std::optional<Expr> initializer
  );
};

struct Declaration {
  DeclVariant decl;
  Declaration(DeclVariant decl);
  Declaration(Declaration &&) = default;
  template <typename T>
  bool is() {
    return std::holds_alternative<T>(decl);
  }
  template <typename T>
  T &as() {
    return std::get<T>(decl);
  }
  std::string s_expr(int cur, int ind);
};

struct Expression {
  Expr expr;
  Expression(Expr expr);
};

struct For {};

struct Return {
  std::optional<Expr> value;
};

struct While {};

struct Print {
  std::vector<Expr> args;
  bool newline;
};

using StmtVariant = std::variant<
    std::monostate, std::unique_ptr<Assign>, std::unique_ptr<Break>,
    std::unique_ptr<Continue>, Declaration, std::unique_ptr<Expression>,
    std::unique_ptr<For>, std::unique_ptr<Return>, std::unique_ptr<While>,
    std::unique_ptr<Print>>;

struct Stmt {
  StmtVariant node;
  template <typename T>
  bool is() {
    return std::holds_alternative<T>(node);
  }
  template <typename T>
  T &as() {
    return std::get<T>(node);
  }
  std::string s_expr(int cur, int ind);
};

struct AST {
  std::vector<Declaration> decls;

  std::unique_ptr<Namespace> globals;

  AST(std::vector<Declaration> decls, std::unique_ptr<Namespace> globals);

  std::string to_string();
};

} // namespace cinnabar
