#pragma once

#include "lexer.hpp"
#include "ast.hpp"

#include <memory>
#include <optional>

namespace cinnabar {


class Parser {
  struct ParseError { };

  bool has_error = false;
  Lexer lexer;
  std::span<Token> tokens;

  int current = 0;
  std::vector<Namespace *> namespaces;
  std::vector<Type> types;

  TypeId reserve_new_type(Token &name);
  void link_type(Token &name, TypeDeclPtr type_decl_ptr);
  void reserve_fun(Token &name);
  void link_fun(Token &name, FunDecl *fun_decl);
  void reserve_var(Token &name);
  void link_var(Token &name, VarDecl *var_decl);

  void add_type(Token &name, TypeId id);
  void add_fun(Token &name, FunDecl *fun_decl);
  void add_var(Token &name, VarDecl *var_decl);

  bool is_at_end();
  Token cur_token();
  bool check(Lexeme l);
  // returns token with relative index i to current
  Token peek(int i);
  // if current token matches l, advance and return true, else false
  bool match(Lexeme l);
  bool match(const std::vector<Lexeme> &lexemes);
  Token expect(Lexeme l, std::string_view message);
  Token advance();

  ParseError error_at(Token &t, std::string_view message);
  ParseError error_prev(std::string_view message);
  ParseError error_cur(std::string_view message);
  void synchronize();

  GenType type_name();
  std::pair<Token, GenType> ident_type();

  Stmt toplevel_declaration();
  std::unique_ptr<StructDecl> struct_declaration();
  std::unique_ptr<EnumDecl> enum_declaration();
  std::unique_ptr<FunDecl> function_declaration();
  std::unique_ptr<VarDecl> variable_declaration();
  
  Stmt statement();
  
  std::unique_ptr<Block> block();
  // block with existing namespace (for functions)
  // namespace numbering will clash, handle this elsewhere...
  std::unique_ptr<Block> block(Namespace &namesp);
  
  Stmt expression_statement();
  // expression with min binding power
  Expr expression_bp(int min_bp);
  int prefix_binding_power(Lexeme l);
  int infix_binding_power(Lexeme l);
  int postfix_binding_power(Lexeme l);

  std::vector<Expr> argument_list();

public:
  Parser(std::string source);
  AST parse();
};

} // namespace cinnabar
