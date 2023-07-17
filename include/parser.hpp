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
  void add_name(Token &name, size_t type, int index);
  void add_name(Declaration &declaration, int index);

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

  TypeName type_name();
  std::pair<Token, TypeName> ident_type();

  Stmt toplevel_declaration();
  StructDecl struct_declaration();
  EnumDecl enum_declaration();
  FunDecl function_declaration();
  VarDecl variable_declaration();
  
  Stmt statement();
  
  Block block();
  // block with existing namespace (for functions)
  // namespace numbering will clash, handle this elsewhere...
  Block block(Namespace &namesp);
  
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
