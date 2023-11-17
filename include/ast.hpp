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
struct Variable;
struct Unary;

struct BuiltinType;

extern const std::vector<std::string_view> default_primitives;

using DeclVariant = std::variant<
    std::unique_ptr<EnumDecl>, std::unique_ptr<FunDecl>,
    std::unique_ptr<StructDecl>, std::unique_ptr<VarDecl>>;

struct Primitive {
  Token name;
};

using DeclPtr =
    std::variant<Primitive *, BuiltinType *, EnumDecl *, FunDecl *, StructDecl *, VarDecl *>;
using TypeDeclPtr = std::variant<Primitive *, BuiltinType *, EnumDecl *, StructDecl *>;

// Array[T, N]
struct GenericName {
  std::string base_name;
  std::vector<std::string> params;
  [[nodiscard]] bool is_concrete() const;
};

// Array[Array[i32, 4], 8]
struct GenericInst {
  std::string base_name;
  std::vector<GenericInst> args;

  GenericInst(std::string_view base_name);
  GenericInst(std::string_view base_name, std::vector<GenericInst> args);

  [[nodiscard]] std::string to_string() const;
  [[nodiscard]] bool is_concrete() const;
};

struct BuiltinType {
  Token name;
  GenericName name_param;
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
    std::unique_ptr<Literal>, std::unique_ptr<Unary>, std::unique_ptr<Variable>>;

struct Expr {
  ExprVariant node;
  Expr(ExprVariant node);

  template <typename T> bool is() { return std::holds_alternative<T>(node); }
  template <typename T> T &as() { return std::get<T>(node); }
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
  FunDecl *fun_decl = nullptr; // change to Variable?
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
  std::vector<Expr> args;
};

using LiteralVariant =
    std::variant<int32_t, int64_t, float, double, bool, char, std::string>;

struct Literal {
  LiteralVariant val;
  Literal(LiteralVariant val);
};

struct Variable {
  Token name;
  Variable(Token name);
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

// enum variants, struct fields
struct TypedName {
  Token name;
  GenericInst gentype;
  inline TypedName(Token name, GenericInst gentype): name(std::move(name)), gentype(std::move(gentype)) {}
};

struct EnumDecl {
  Token name;
  GenericName name_param;
  std::vector<TypedName> variants; // unit gentype for empty
  std::vector<std::unique_ptr<FunDecl>> methods;
  std::unique_ptr<Namespace> namesp;
  EnumDecl(
      Token name, GenericName name_param, std::vector<TypedName> variants,
      std::vector<std::unique_ptr<FunDecl>> methods, std::unique_ptr<Namespace> namesp
  );
};

struct FunDecl {
  Token name;
  GenericName name_param;
  std::vector<std::unique_ptr<VarDecl>> params;
  GenericInst return_type;
  std::unique_ptr<Block> body;
  TypeDeclPtr method_of = static_cast<BuiltinType *>(nullptr);
  FunDecl(
      Token name, GenericName name_param, std::vector<std::unique_ptr<VarDecl>> params,
      GenericInst return_type, std::unique_ptr<Block> body
  );
  std::string s_expr(int cur, int ind);
};

struct StructDecl {
  Token name;
  GenericName name_param;
  std::vector<TypedName> fields;
  std::vector<std::unique_ptr<FunDecl>> methods;
  std::unique_ptr<Namespace> namesp;
  StructDecl(
      Token name, GenericName name_param, std::vector<TypedName> fields,
      std::vector<std::unique_ptr<FunDecl>> methods, std::unique_ptr<Namespace> namesp
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
  template <typename T> bool is() { return std::holds_alternative<T>(decl); }
  template <typename T> T &as() { return std::get<T>(decl); }
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
    std::unique_ptr<Print>
>;

struct Stmt {
  StmtVariant node;
  template <typename T> bool is() { return std::holds_alternative<T>(node); }
  template <typename T> T &as() { return std::get<T>(node); }
  std::string s_expr(int cur, int ind);
};

struct AST {
  std::vector<Declaration> decls;
  std::vector<std::unique_ptr<Primitive>> primitives;
  std::vector<std::unique_ptr<BuiltinType>> builtin_types;

  std::unique_ptr<Namespace> globals;

  AST(std::vector<Declaration> decls,
      std::vector<std::unique_ptr<Primitive>> primitives,
      std::vector<std::unique_ptr<BuiltinType>> builtin_types,
      std::unique_ptr<Namespace> globals);

  std::string to_string();
};

} // namespace cinnabar
