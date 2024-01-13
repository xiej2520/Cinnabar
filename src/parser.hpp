#pragma once

#include "lexer.hpp"
#include "ast.hpp"

#include <memory>
#include <optional>

namespace cinnabar {

class Parser {
  struct ParseError {
    std::vector<std::pair<Token, std::string>> where_errors;
  };

  bool has_error = false;
  Lexer lexer;
  std::span<Token> tokens;

  int current = 0;
  std::vector<Namespace *> namespaces;

  // reserves name with nullptr
  template<typename T>
  void reserve_name(const Token &name);
  // links a declaration with a previously reserved name
  template<typename T>
  void link_name(const Token &name, T decl);
  // adds a name and its declaration
  template<typename T>
  void add_name(const Token &name, T decl);

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

  ParseError error_at(const Token &t, std::string_view message);
  ParseError error_prev(std::string_view message);
  ParseError error_cur(std::string_view message);
  void synchronize();

  GenericSignature generic_signature();
  GenericInst generic_instance();
  std::pair<Token, GenericInst> name_type();

  Stmt toplevel_declaration();
  std::unique_ptr<StructDecl> struct_declaration();
  std::unique_ptr<EnumDecl> enum_declaration();
  std::unique_ptr<FunDecl> function_declaration();
  std::unique_ptr<VarDecl> variable_declaration();
  
  Stmt statement();
  
  std::unique_ptr<Block> block();
  // block with existing namespace (for functions)
  // namespace numbering will clash, handle this elsewhere...
  std::unique_ptr<Block> block(std::unique_ptr<Namespace> namesp);
  
  char parse_character(std::string_view literal);
  
  Stmt expression_statement();
  // expression with min binding power
  Expr expression_bp(int min_bp);
  Expr expression_lhs(const Token &token);
  int prefix_binding_power(Lexeme l);
  int infix_binding_power(Lexeme l);
  int postfix_binding_power(Lexeme l);

  std::vector<Expr> argument_list();
  std::vector<Expr> index_argument_list();

public:
  Parser(std::string source);
  AST parse();
  inline bool get_has_error() { return has_error; }
};

} // namespace cinnabar
