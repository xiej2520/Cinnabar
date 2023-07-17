#pragma once
#include "ast_ops.hpp"
#include "common.hpp"
#include "lexer.hpp"

#include <memory>
#include <optional>
#include <unordered_set>
#include <variant>
#include <vector>

/*
Statements
// Assignment
// Block
// Break
// Continue
// Declaration
// Expression
// // Binary
// // Call
// // Grouping
// // If
// // Literal
// // Unary
// // Variable
// For
// Function
// Return
// While
*/

/*
C++2
primary_expression_node
  expression_list_node
  id_expression_node
    unqualified_id_node
    qualified_id_node
  declaration_node
  inspect_expression_node
  literal_node

term
  expression_node
  type_id_node
    unqualified_id_node
    qualified_id_node

statement_node
  expression_statement_node
  compound_statement_node
  selection_statement_node
  declaration_node
    function_type_node
      single_type_id
      parameter_declaration_list_node
    type_id_node
    type_node
    namespace_node
    alias_node
      type_id_node
      id_expression_node
      expression_node
  return_statement_node
  iteration_statement_node
  contract_node
  inspect_expression_node
  jump_statement_node

binary_expression_node

is_as_expression_node
  prefix_expression_node
    postfix_expression_node

assignment_expression_lhs_rhs
expression_list_node
  term

capture
capture_group

alternative_node

parameter_declaration_node
parameter_declaration_list_node

type_node
namespace_node

translation_unit_node
*/

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

using DeclVariant =
    std::variant<std::unique_ptr<EnumDecl>, std::unique_ptr<FunDecl>,
        std::unique_ptr<StructDecl>, std::unique_ptr<VarDecl>>;

struct BuiltinType {
  std::string name;
};

using DeclPtr =
    std::variant<BuiltinType *, EnumDecl *, FunDecl *, StructDecl *, VarDecl *>;
using TypeDeclPtr = std::variant<BuiltinType *, EnumDecl *, StructDecl *>;

using TypeId = int;

struct GenType {
  std::string name;
  std::vector<GenType> params;
  std::string to_string();
};

struct Type {
  TypeDeclPtr type_decl_ptr;
  TypeId id;
  template <typename T> bool is() {
    return std::holds_alternative<T>(type_decl_ptr);
  }
  template <typename T> T &as() { return std::get<T>(type_decl_ptr); }
  std::string_view name();
};

struct Namespace {
  std::unordered_set<std::string_view> names;
  std::unordered_map<std::string_view, TypeId> type_decls;
  std::unordered_map<std::string_view, FunDecl *> fun_decls;
  std::unordered_map<std::string_view, VarDecl *> var_decls;
  Namespace() = default;
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

using ExprVariant = std::variant<std::monostate, std::unique_ptr<Binary>,
    std::unique_ptr<Block>, std::unique_ptr<DotRef>, std::unique_ptr<FunCall>,
    std::unique_ptr<If>, std::unique_ptr<Literal>, std::unique_ptr<Unary>,
    std::unique_ptr<Variable>>;

struct Expr {
  ExprVariant node;
  Expr(ExprVariant node);
  template <typename T> bool is() {
    return std::holds_alternative<T>(node);
  }
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
  Namespace namesp;
  Block(std::vector<Stmt> stmts, Namespace namesp);
};

struct DotRef {
  TypeId type = -1;
  Expr lvalue;
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

struct EnumDecl {
  Token name;
  std::vector<std::pair<Token, GenType>> variants;
  std::vector<std::unique_ptr<FunDecl>> methods;
  Namespace namesp;
  EnumDecl(Token name, std::vector<std::pair<Token, GenType>> variants,
      std::vector<std::unique_ptr<FunDecl>> methods, Namespace namesp);
};

struct FunDecl {
  Token name;
  std::vector<std::unique_ptr<VarDecl>> params;
  std::unique_ptr<Block> body;
  FunDecl(Token name, std::vector<std::unique_ptr<VarDecl>> params,
      std::unique_ptr<Block> body);
  std::string s_expr(int cur, int ind);
};

struct StructDecl {
  Token name;
  std::vector<std::pair<Token, GenType>> fields;
  std::vector<std::unique_ptr<FunDecl>> methods;
  Namespace namesp;
  StructDecl(Token name, std::vector<std::pair<Token, GenType>> fields,
      std::vector<std::unique_ptr<FunDecl>> methods, Namespace namesp);
};

struct VarDecl {
  Token name;
  std::optional<GenType> type_specifier;
  Expr initializer;
  VarDecl(Token name, std::optional<GenType> type_specifier, Expr initializer);
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

using StmtVariant = std::variant<std::monostate, std::unique_ptr<Assign>,
    std::unique_ptr<Break>, std::unique_ptr<Continue>, Declaration,
    std::unique_ptr<Expression>, std::unique_ptr<For>, std::unique_ptr<Return>,
    std::unique_ptr<While>>;

struct Stmt {
  StmtVariant node;
  template <typename T> bool is() { return std::holds_alternative<T>(node); }
  template <typename T> T &as() { return std::get<T>(node); }
  std::string s_expr(int cur, int ind);
};

struct AST {
  std::vector<Declaration> decls;

  std::vector<Type> types;
  std::vector<std::unique_ptr<BuiltinType>> builtin_types;

  Namespace globals;

  AST(std::vector<Declaration> decls, Namespace globals);

  std::string to_string();
};

} // namespace cinnabar
