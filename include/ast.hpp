#pragma once
#include "ast_ops.hpp"
#include "common.hpp"
#include "lexer.hpp"

#include <memory>
#include <optional>
#include <unordered_set>
#include <variant>
#include <vector>

namespace cinnabar {

extern std::vector<std::string> default_builtin_types;

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
struct Literal;
struct Variable;
struct Unary;

using DeclVariant = std::variant<
    std::unique_ptr<EnumDecl>, std::unique_ptr<FunDecl>,
    std::unique_ptr<StructDecl>, std::unique_ptr<VarDecl>>;

struct BuiltinType {
  std::string name_str; // Token holds string_view, needs owning string
  Token name;
  BuiltinType(std::string name);
};

using DeclPtr =
    std::variant<BuiltinType *, EnumDecl *, FunDecl *, StructDecl *, VarDecl *>;
using TypeDeclPtr = std::variant<BuiltinType *, EnumDecl *, StructDecl *>;

using TypeId = int;

struct GenType {
  std::string name;
  std::vector<GenType> params;

  GenType(std::string_view name);
  GenType(std::string_view name, std::vector<GenType> params);
  std::string to_string() const;
};

struct Type {
  TypeDeclPtr type_decl_ptr;
  TypeId id;
  GenType concrete_type;
  
  Type(TypeDeclPtr decl, TypeId id, GenType concrete_type);

  template <typename T> bool is() {
    return std::holds_alternative<T>(type_decl_ptr);
  }
  template <typename T> T &as() { return std::get<T>(type_decl_ptr); }
  std::string name() const;
};

struct Namespace {
  std::unordered_map<std::string_view, DeclPtr> names;
  // concrete types don't always have tokens, own the strings
  std::unordered_map<std::string, TypeId> concrete_types;
  
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
    std::unique_ptr<FunCall>, std::unique_ptr<If>, std::unique_ptr<Literal>,
    std::unique_ptr<Unary>, std::unique_ptr<Variable>>;

struct Expr {
  ExprVariant node;
  Expr(ExprVariant node);

  TypeId type();

  template <typename T> bool is() { return std::holds_alternative<T>(node); }
  template <typename T> T &as() { return std::get<T>(node); }
  std::string s_expr(int cur, int ind); // current indent, indent
};

struct Binary {
  TypeId type = -1;
  BinaryOp op;
  Expr left;
  Expr right;
  Binary(BinaryOp op, Expr left, Expr right);
};

struct Block {
  TypeId type = -1;
  std::vector<Stmt> stmts;
  std::unique_ptr<Namespace> namesp;
  Block(std::vector<Stmt> stmts, std::unique_ptr<Namespace> namesp);
  std::string to_string(int cur, int ind);
};

struct DotRef {
  TypeId type = -1;
  Expr lvalue;
  // var.f()
  FunDecl *fun_decl = nullptr; // change to Variable?
  Token name;
  DotRef(Expr lvalue, Token name);
};

struct FunCall {
  TypeId type = -1;
  Expr callee;
  std::vector<Expr> args;
  FunCall(Expr callee, std::vector<Expr> args);
};

struct If {
  TypeId type = -1;
  struct Branch {
    // else branch will be a Literal true condition as the last block
    Expr condition;
    std::unique_ptr<Block> block;
    Branch(Expr condition, std::unique_ptr<Block> block);
  };
  std::vector<std::unique_ptr<Branch>> branches;
  If(std::vector<std::unique_ptr<Branch>> branches);
};

using LiteralVariant =
    std::variant<i32, i64, f32, f64, bool, char, std::string>;

struct Literal {
  TypeId type = -1;
  LiteralVariant val;
  Literal(LiteralVariant val);
};

struct Variable {
  TypeId type = -1;
  Token name;
  DeclPtr decl = static_cast<BuiltinType *>(nullptr);
  Variable(Token name);
};

struct Unary {
  TypeId type = -1;
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
  GenType gentype;
  TypeId type = -1;
  inline TypedName(Token name, GenType gentype): name(std::move(name)), gentype(std::move(gentype)) {}
};

struct EnumDecl {
  Token name;
  std::vector<TypedName> variants; // unit gentype for empty
  std::vector<std::unique_ptr<FunDecl>> methods;
  std::unique_ptr<Namespace> namesp;
  EnumDecl(
      Token name, std::vector<TypedName> variants,
      std::vector<std::unique_ptr<FunDecl>> methods, std::unique_ptr<Namespace> namesp
  );
};

struct FunDecl {
  Token name;
  std::vector<std::unique_ptr<VarDecl>> params;
  GenType return_type;
  std::unique_ptr<Block> body;
  TypeDeclPtr method_of = static_cast<BuiltinType *>(nullptr);
  FunDecl(
      Token name, std::vector<std::unique_ptr<VarDecl>> params,
      GenType return_type, std::unique_ptr<Block> body
  );
  std::string s_expr(int cur, int ind);
};

struct StructDecl {
  Token name;
  std::vector<TypedName> fields;
  std::vector<std::unique_ptr<FunDecl>> methods;
  std::unique_ptr<Namespace> namesp;
  StructDecl(
      Token name, std::vector<TypedName> fields,
      std::vector<std::unique_ptr<FunDecl>> methods, std::unique_ptr<Namespace> namesp
  );
};

struct VarDecl {
  Token name;
  TypeId type = -1;
  std::optional<GenType> type_specifier;
  std::optional<Expr> initializer;
  VarDecl(
      Token name, std::optional<GenType> type_specifier,
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
struct Return {};
struct While {};

using StmtVariant = std::variant<
    std::monostate, std::unique_ptr<Assign>, std::unique_ptr<Break>,
    std::unique_ptr<Continue>, Declaration, std::unique_ptr<Expression>,
    std::unique_ptr<For>, std::unique_ptr<Return>, std::unique_ptr<While>>;

struct Stmt {
  StmtVariant node;
  template <typename T> bool is() { return std::holds_alternative<T>(node); }
  template <typename T> T &as() { return std::get<T>(node); }
  std::string s_expr(int cur, int ind);
};

struct AST {
  std::vector<Declaration> decls;

  std::vector<FunDecl *> fun_ptrs;
  std::vector<Type> types;
  std::vector<std::unique_ptr<BuiltinType>> builtin_types;

  std::unique_ptr<Namespace> globals;

  AST(std::vector<Declaration> decls,
      std::vector<std::unique_ptr<BuiltinType>> builtin_types,
      std::unique_ptr<Namespace> globals);

  std::string to_string();
};

} // namespace cinnabar
