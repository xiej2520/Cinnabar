#include "parser.hpp"

#include "fmt/core.h"

namespace cinnabar {

using enum Lexeme;

Parser::Parser(std::string source) : lexer(std::move(source)) {
  lexer.lex();
  tokens = lexer.get_tokens();
}

void Parser::add_name(Token &name, size_t type, int index) {
  Namespace *current = namespaces.back();
  if (current->names.contains(name.str)) {
    throw error_at(name, fmt::format("Redefinition of previous name \'{}\'.", name.str));
  }
  current->names[name.str] = {type, index};
}

template <class... Ts> struct overload : Ts... { using Ts::operator()...; };
template <class... Ts> overload(Ts...) -> overload<Ts...>; // helloooo clang???
void Parser::add_name(Declaration &declaration, int index) {
  return std::visit(overload{
    [&](EnumDecl &decl) { return add_name(decl.name, variant_index<DeclVariant, EnumDecl>(), index); },
    [&](FunDecl &decl) { return add_name(decl.name, variant_index<DeclVariant, FunDecl>(), index); },
    [&](StructDecl &decl) { return add_name(decl.name, variant_index<DeclVariant, StructDecl>(), index); },
    [&](VarDecl &decl) { return add_name(decl.name, variant_index<DeclVariant, VarDecl>(), index); },
  }, declaration.decl);
}


bool Parser::is_at_end() { return cur_token().lexeme == Lexeme::END_OF_FILE; }

Token Parser::cur_token() { return tokens[current]; }

Token Parser::peek(int i) { return tokens[current + i]; }

bool Parser::match(Lexeme l) {
  if (check(l)) {
    advance();
    return true;
  }
  return false;
}

bool Parser::match(const std::vector<Lexeme> &lexemes) {
  for (Lexeme l : lexemes) {
    if (check(l)) {
      advance();
      return true;
    }
  }
  return false;
}

Token Parser::expect(Lexeme l, std::string_view message) {
  if (cur_token().lexeme == l) {
    advance();
    return peek(-1);
  }
  throw error_cur(message);
}

bool Parser::check(Lexeme l) {
  if (is_at_end())
    return false;
  return cur_token().lexeme == l;
}

Token Parser::advance() {
  if (!is_at_end()) {
    current++;
  }
  return peek(-1);
}

Parser::ParseError Parser::error_at(Token &t, std::string_view message) {
  fmt::print("Line {}, column {}, {}\n", t.line, t.col, message);
  has_error = true;
  return ParseError{};
}

Parser::ParseError Parser::error_prev(std::string_view message) {
  Token t = peek(-1);
  fmt::print("Line {}, column {}, {}\n", t.line, t.col, message);
  has_error = true;
  return ParseError{};
}

Parser::ParseError Parser::error_cur(std::string_view message) {
  Token t = cur_token();
  fmt::print("Line {}, column {}, {}\n", t.line, t.col, message);
  has_error = true;
  return ParseError{};
}

void Parser::synchronize() {
  while (!is_at_end()) {
    switch (cur_token().lexeme) {
    case SEMICOLON:
      advance(); // go past semicolon
    // try to parse again if we see these
    // 'if' not included because it's an expr
    case STRUCT:
    case FOR:
    case FUN:
    case LET:
    case RETURN:
    case VAR:
    case WHILE:
      return;
    default:
      break;
    }
    advance();
  }
}

TypeName Parser::type_name() {
  return TypeName(expect(IDENTIFIER, "Expected type name.").str);
}

std::pair<Token, TypeName> Parser::ident_type() {
  std::pair<Token, TypeName> res;
  res.first = expect(IDENTIFIER, "Expected identifier.");
  res.second = type_name();
  return res;
}

Stmt Parser::toplevel_declaration() {
  try {
    if (match(STRUCT)) {
      return Stmt{Declaration{struct_declaration()}};
    } else if (match(ENUM)) {
      return Stmt{Declaration{enum_declaration()}};
    } else if (match(FUN)) {
      return Stmt{Declaration{function_declaration()}};
    } else if (match({LET, VAR})) {
      return Stmt{Declaration{variable_declaration()}};
    } else if (match(SEMICOLON)) {
      return Stmt{std::monostate{}};
    } else {
      throw error_cur("Expected top-level declaration.");
    }
  } catch (ParseError &err) {
    synchronize();
  }
  return Stmt{std::monostate{}};
}

StructDecl Parser::struct_declaration() {
  Token name = expect(IDENTIFIER, "Expected struct name.");
  expect(LEFT_BRACE, "Expect '{' before struct body.");

  std::vector<std::pair<Token, TypeName>> fields;
  std::vector<FunDecl> methods;
  Namespace namesp;
  namespaces.push_back(&namesp);
  add_name(name, variant_index<DeclVariant, StructDecl>(), -1);
  try {
    while (!check(RIGHT_BRACE) && !is_at_end()) {
      if (match(FUN)) {
        methods.emplace_back(function_declaration());
        add_name(methods.back().name, variant_index<DeclVariant, FunDecl>(), methods.size() - 1);
      } else if (match(SEMICOLON)) {
        // skip
      } else {
        fields.emplace_back(ident_type());
        add_name(fields.back().first, variant_index<DeclVariant, VarDecl>(), fields.size() - 1);
      }
    }
  }
  catch (ParseError &err) {
    namespaces.pop_back();
    throw err;
  }

  namespaces.pop_back();
  expect(RIGHT_BRACE, "Expect '}' after struct body");
  return StructDecl{name, fields, std::move(methods), namesp};
}

EnumDecl Parser::enum_declaration() {
  Token name = expect(IDENTIFIER, "Expected enum name.");
  expect(LEFT_BRACE, "Expect '{' before enum body.");

  std::vector<std::pair<Token, TypeName>> variants;
  std::vector<FunDecl> methods;
  Namespace namesp;
  namespaces.push_back(&namesp);
  add_name(name, variant_index<DeclVariant, EnumDecl>(), -1);
  try {
    while (!check(RIGHT_BRACE) && !is_at_end()) {
      if (match(FUN)) {
        methods.emplace_back(function_declaration());
        add_name(methods.back().name, variant_index<DeclVariant, FunDecl>(), methods.size() - 1);
      } else if (match(SEMICOLON)) {
        // continue
      } else {
        std::pair<Token, TypeName> res;
        res.first = expect(IDENTIFIER, "Expected identifier.");
        res.second = check(SEMICOLON) ? (advance(), TypeName{"()"}) : type_name();
        variants.emplace_back(res);
        add_name(variants.back().first, variant_index<DeclVariant, VarDecl>(), variants.size() - 1);
      }
    }
  }
  catch (ParseError &err) {
    namespaces.pop_back();
    throw err;
  }

  namespaces.pop_back();
  expect(RIGHT_BRACE, "Expect '}' after enum body.");
  return EnumDecl(name, variants, std::move(methods), namesp);
}

FunDecl Parser::function_declaration() {
  Token name = expect(IDENTIFIER, "Expected function name.");
  expect(LEFT_PAREN, "Expect '(' after function name.");
  
  Namespace namesp;
  namespaces.push_back(&namesp);
  // function can refer to itself in body
  add_name(name, variant_index<DeclVariant, FunDecl>(), -1);
  std::vector<std::pair<Token, TypeName>> parameters;
  Block body({}, namesp);
  try {
    if (!check(RIGHT_PAREN)) {
      do {
        parameters.push_back(ident_type());
        add_name(parameters.back().first, variant_index<DeclVariant, VarDecl>(), parameters.size() - 1);
      } while (match(COMMA));
    }
    expect(RIGHT_PAREN, "Expect ')' after parameters.");
    expect(LEFT_BRACE, "Expect '{' before function body.");
    body = block(namesp);
  }
  catch (ParseError &err) {
    namespaces.pop_back();
    throw err;
  }
  namespaces.pop_back();
  // check for return type
  return FunDecl(name, parameters, std::move(body));
}

VarDecl Parser::variable_declaration() {
  Token name = expect(IDENTIFIER, "Expected identifier.");
  std::optional<TypeName> type;
  std::unique_ptr<Expr> initializer = nullptr;
  if (!check(EQUAL)) {
    type = type_name();
  }
  if (match(EQUAL)) {
    initializer = std::make_unique<Expr>(expression_bp(0));
  }
  expect(SEMICOLON, "Expect ';' (EOL) after variable declaration.");
  return VarDecl(name, type, std::move(initializer));
}

Stmt Parser::statement() {
  try {
    if (match(STRUCT)) {
      return Stmt{Declaration{struct_declaration()}};
    } else if (match(ENUM)) {
      return Stmt{Declaration{enum_declaration()}};
    } else if (match(FUN)) {
      return Stmt{Declaration{function_declaration()}};
    } else if (match({LET, VAR})) {
      return Stmt{Declaration{variable_declaration()}};
    } else if (match(SEMICOLON)) {
      return Stmt{std::monostate{}};
    }
    else if (match(FOR)) {
    }
    else {
      return expression_statement();
    }
  } catch (ParseError &err) {
    synchronize();
  }
  return Stmt{std::monostate{}};
}

Block Parser::block() {
  std::vector<Stmt> res;
  Namespace namesp;
  namespaces.push_back(&namesp);
  try {
    while (!check(RIGHT_BRACE) && !is_at_end()) {
      Stmt stmt = statement();
      if (!stmt.is<std::monostate>()) {
        res.emplace_back(std::move(stmt));
      }
      if (stmt.is<Declaration>()) {
        add_name(stmt.as<Declaration>(), res.size()-1);
      }
    }
    expect(RIGHT_BRACE, "Expected '}' after block.");
  }
  catch (ParseError &err) {
    namespaces.pop_back();
  }
  namespaces.pop_back();
  return Block{std::move(res), namesp};
}

Block Parser::block(Namespace &namesp) {
  std::vector<Stmt> res;
  while (!check(RIGHT_BRACE) && !is_at_end()) {
    Stmt stmt = statement();
    if (!stmt.is<std::monostate>()) {
      res.emplace_back(std::move(stmt));
    }
    if (stmt.is<Declaration>()) {
      add_name(stmt.as<Declaration>(), res.size() - 1);
    }
  }
  expect(RIGHT_BRACE, "Expected '}' after block.");
  return Block{std::move(res), namesp};
}

Stmt Parser::expression_statement() {
  Expr expr = expression_bp(0);
  switch (cur_token().lexeme) {
  case Lexeme::EQUAL:
  case Lexeme::PLUS_EQUAL:
  case Lexeme::MINUS_EQUAL:
  case Lexeme::STAR_EQUAL:
  case Lexeme::SLASH_EQUAL:
  case Lexeme::MODULO_EQUAL:
  case Lexeme::AMPERSAND_EQUAL:
  case Lexeme::BAR_EQUAL:
  case Lexeme::CARET_EQUAL: {
    AssignOp op = to_assignop(advance().lexeme);
    if (op == AssignOp::ERROR) {
      throw error_prev("How did this happen?");
    }
    Expr rhs = expression_bp(0);
    return Stmt{Assign{op, std::make_unique<Expr>(std::move(expr)),
                       std::make_unique<Expr>(std::move(rhs))}};
  }
  default:
    break;
  }
  expect(SEMICOLON, "Expect ';' (EOL) after expression.");
  return Stmt{Expression{std::move(expr)}};
}

// parses expression above minimum binding power argument
// only consume tokens that are in expression!
Expr Parser::expression_bp(int min_bp) {
  Token token = advance();
  std::optional<Expr> lhs = std::nullopt;
  // parse LHS of expression
  if (prefix_binding_power(token.lexeme) != -1) {
    lhs = Expr{Unary{to_unaryop(token.lexeme),
                     std::make_unique<Expr>(
                         expression_bp(prefix_binding_power(token.lexeme)))}};
  }
  switch (token.lexeme) {
  // clang-format off
    case TRUE: lhs = Expr{Literal{true}}; break;
    case FALSE: lhs = Expr{Literal{false}}; break;
    case INTEGER: lhs = Expr{Literal{std::stoi(std::string(token.str))}}; break;
    case DECIMAL: lhs = Expr{Literal{std::stod(std::string(token.str))}}; break;
    case STRING: lhs = Expr{Literal{std::string{token.str}}}; break;
    case CHARACTER: lhs = Expr{Literal{token.str[1]}}; break; // fix later
    case IDENTIFIER: lhs = Expr{Variable{token}}; break;
    case IF: {
      std::vector<If::Branch> branches;
      Expr condition = expression_bp(0);
      expect(LEFT_BRACE, "Expected '{' after 'if'.");
      Block b = block();
      branches.emplace_back(If::Branch(std::make_unique<Expr>(std::move(condition)), std::move(b)));
      while (match(ELSE)) {
        if (match(IF)) {
          branches.emplace_back(If::Branch(std::make_unique<Expr>(expression_bp(0)), block()));
        }
        else {
          expect(LEFT_BRACE, "Expected '{' after 'else'.");
          branches.emplace_back(If::Branch(std::make_unique<Expr>(Literal{true}), block()));
          break;
        }
      }
      lhs = Expr{If{std::move(branches)}};
      break;
    }
    case ELSE: {
      throw error_prev("Unexpected 'else' with no preceding 'if'.");
    }
    case LEFT_BRACE: {
      lhs = Expr{Block{block()}};
      break;
    }
    default: throw error_prev(fmt::format("Unexpected token in expression: {}.", to_string(token.lexeme)));
    // clang-format on
  }

  // parse postfix or infix with min_bp or greater
  while (cur_token().lexeme != END_OF_FILE) {
    token = cur_token();
    int lhs_postfix_bp = postfix_binding_power(token.lexeme);
    if (lhs_postfix_bp != -1) {      // is a postfix op
      if (lhs_postfix_bp < min_bp) { // not enough binding power
        break;
      }
      switch (token.lexeme) {
      case LEFT_PAREN: {
        advance();
        std::vector<Expr> args = argument_list();
        lhs = Expr{FunCall{std::make_unique<Expr>(std::move(lhs.value())),
                           std::move(args)}};
        break;
      }
      case DOT: {
        advance();
        Token ident = expect(IDENTIFIER, "Expected identifier after '.'.");
        lhs =
            Expr{DotRef{std::make_unique<Expr>(std::move(lhs.value())), ident}};
        break;
      }
      default:
        advance();
        break;
      }
      continue;
    }
    int lhs_infix_bp = infix_binding_power(token.lexeme);
    if (lhs_infix_bp != -1) { // is an infix op
      if (lhs_infix_bp < min_bp) {
        break;
      }
      advance();
      BinaryOp op = to_binop(token.lexeme);
      // recursive call with slightly higher binding power for
      // left-associativity
      auto rhs = expression_bp(min_bp + 1);
      // if assign was an Expr, would do switch here
      lhs = Expr{Binary{op, std::make_unique<Expr>(std::move(lhs.value())),
                        std::make_unique<Expr>(std::move(rhs))}};
      continue;
    }
    // not postfix or infix, end
    break;
  }
  if (!lhs.has_value()) {
    throw error_prev("Expected expression.");
  }
  return std::move(lhs.value());
}

enum BindingPower {
  NONE,
  OR,         // ||
  AND,        // &&
  EQUALITY,   // == !=
  COMPARISON, // < > <= >=
  BITWISE,    // & | ^ << >>
  TERM,       // + -
  FACTOR,     // * /
  UNARY,      // ! + -
  CALL,       // . ()
  PRIMARY,
  POSTFIX,
};

int Parser::prefix_binding_power(Lexeme l) {
  switch (l) {
  case PLUS:
  case MINUS:
  case BANG:
    return UNARY;
  default:
    return -1;
  }
}

int Parser::infix_binding_power(Lexeme l) {
  switch (l) {
  case BAR_BAR:
    return OR;
  case AMPERSAND_AMPERSAND:
    return AND;
  case EQUAL_EQUAL:
  case BANG_EQUAL:
    return EQUALITY;
  case LESS:
  case GREATER:
  case LESS_EQUAL:
  case GREATER_EQUAL:
    return COMPARISON;
  case AMPERSAND:
  case BAR:
  case CARET:
  case LESS_LESS:
  case GREATER_GREATER:
    return BITWISE;
  case PLUS:
  case MINUS:
    return TERM;
  case STAR:
  case SLASH:
  case MODULO:
    return FACTOR;
  case DOT:
  case LEFT_PAREN:
    return CALL;
  default:
    return -1;
  }
}

int Parser::postfix_binding_power(Lexeme l) {
  switch (l) {
  case DOT:
  case DOT_DOT:
  case DOLLAR:
  case LEFT_PAREN:
    return POSTFIX;
  default:
    return -1;
  }
}

std::vector<Expr> Parser::argument_list() {
  std::vector<Expr> args;
  if (!check(RIGHT_PAREN)) {
    // named arguments could go here
    do {
      args.push_back(expression_bp(0));
    } while (match(COMMA));
  }
  expect(RIGHT_PAREN, "Expect ')' after arguments.");
  return args;
}

AST Parser::parse() {
  std::vector<Declaration> decls;
  Namespace globals;
  namespaces.push_back(&globals);
  while (!is_at_end()) {
    Stmt decl = toplevel_declaration();
    if (!decl.is<std::monostate>()) {
      decls.emplace_back(std::move(decl.as<Declaration>()));
      add_name(decl.as<Declaration>(), decls.size() - 1);
    }
  }
  return AST(std::move(decls), globals);
}

} // namespace cinnabar
