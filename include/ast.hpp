#pragma once
#include "common.hpp"
#include "lexer.hpp"
#include "ast_ops.hpp"

#include <memory>
#include <optional>
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

struct TypeName {
  std::string_view name;
  TypeName() : name("") {}
  TypeName(std::string_view name) : name(name) {}
};

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

using DeclVariant = std::variant<EnumDecl, FunDecl, StructDecl, VarDecl>;

struct Namespace {
  // name -> { typeof name, index of name in the node's vector (-1 for fun/struct own name) }
  // type is using variant_index<DeclVariant, type>()
  std::unordered_map<std::string_view, std::pair<size_t, int>> names;
};

template<typename VariantType, typename T, std::size_t index = 0>
constexpr std::size_t variant_index() {
    static_assert(std::variant_size_v<VariantType> > index, "Type not found in variant");
    if constexpr (index == std::variant_size_v<VariantType>) {
        return index;
    } else if constexpr (std::is_same_v<std::variant_alternative_t<index, VariantType>, T>) {
        return index;
    } else {
        return variant_index<VariantType, T, index + 1>();
    }
} 

//
// expressions
//
struct Binary {
  BinaryOp op;
  std::unique_ptr<Expr> left;
  std::unique_ptr<Expr> right;
  Binary(BinaryOp op, std::unique_ptr<Expr> left, std::unique_ptr<Expr> right);
};

struct Block {
  std::vector<Stmt> stmts;
  Namespace namesp;
  Block(std::vector<Stmt> stmts, Namespace namesp);
};

struct DotRef {
  std::unique_ptr<Expr> lvalue;
  Token name;
  DotRef(std::unique_ptr<Expr> lvalue, Token name);
};

struct FunCall {
  std::unique_ptr<Expr> callee;
  std::vector<Expr> args;
  FunCall(std::unique_ptr<Expr> callee, std::vector<Expr> args);
};

struct If {
  struct Branch {
    std::unique_ptr<Expr> condition;
    Block block;
    Branch(std::unique_ptr<Expr> condition, Block block);
  };
  std::vector<Branch> branches;
  If(std::vector<Branch> branches);
};

using LiteralVariant = std::variant<i32, i64, f32, f64, bool, char, std::string>;

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
  std::unique_ptr<Expr> rhs;
  Unary(UnaryOp op, std::unique_ptr<Expr> rhs);
};

using ExprVariant = std::variant<Binary, Block, DotRef, FunCall, If, Literal, Unary, Variable>;

struct Expr {
  ExprVariant node;
  Expr(ExprVariant node);
  std::string s_expr(int cur, int ind); // current indent, indent
};

//
// statements
//
struct Assign {
  AssignOp op;
  std::unique_ptr<Expr> lhs;
  std::unique_ptr<Expr> rhs;
  Assign(AssignOp op, std::unique_ptr<Expr> lhs, std::unique_ptr<Expr> rhs);
};

struct Break {};
struct Continue {};

struct EnumDecl {
  Token name;
  std::vector<std::pair<Token, TypeName>> variants;
  std::vector<FunDecl> methods;
  Namespace namesp;
  EnumDecl(Token name, std::vector<std::pair<Token, TypeName>> variants,
           std::vector<FunDecl> methods, Namespace namesp);
};

struct FunDecl {
  Token name;
  std::vector<std::pair<Token, TypeName>> params;
  Block body;
  FunDecl(Token name, std::vector<std::pair<Token, TypeName>> params,
          Block body);
  std::string s_expr(int cur, int ind);
};

struct StructDecl {
  Token name;
  std::vector<std::pair<Token, TypeName>> fields;
  std::vector<FunDecl> methods;
  Namespace namesp;
  StructDecl(Token name, std::vector<std::pair<Token, TypeName>> fields,
             std::vector<FunDecl> methods, Namespace namesp);
};

struct VarDecl {
  Token name;
  std::optional<TypeName> type;
  std::unique_ptr<Expr> initializer;
  VarDecl(Token name, std::optional<TypeName> type, std::unique_ptr<Expr> initializer);
};

struct Declaration {
  DeclVariant decl;
  Declaration(DeclVariant decl);
  Declaration(Declaration &&) = default;
  template<typename T>
  bool is() {
    return std::holds_alternative<T>(decl);
  }
  template<typename T>
  T& as() {
    return std::get<T>(decl);
  }
  std::string s_expr(int cur, int ind);
};

struct Expression {
  Expr expr;
  Expression(Expr expr);
};

struct For {};
struct Return {};
struct While {};

using StmtVariant = std::variant<std::monostate, Assign, Break, Continue, Declaration,
                                 Expression, For, Return, While>;

struct Stmt {
  StmtVariant node;
  template<typename T>
  bool is() {
    return std::holds_alternative<T>(node);
  }
  template<typename T>
  T& as() {
    return std::get<T>(node);
  }
  std::string s_expr(int cur, int ind);
};

struct AST {
  std::vector<Declaration> decls;
  Namespace globals;
  AST(std::vector<Declaration> decls, Namespace globals);
  std::string to_string();
};

} // namespace cinnabar
