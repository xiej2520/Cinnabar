#include "parser.hpp"

#include "fmt/core.h"

namespace cinnabar {

using enum Lexeme;

Parser::Parser(std::string source) : lexer(std::move(source)) {
  lexer.lex();
  tokens = lexer.get_tokens();
}

#define RESERVE(table, uninitval)                                              \
  Namespace *current = namespaces.back();                                      \
  if (current->names.contains(name.str)) {                                     \
    throw error_at(                                                            \
        name, fmt::format("Redefinition of previous name \'{}\'.", name.str)); \
  }                                                                            \
  current->names.insert(name.str);                                             \
  current->table[name.str] = uninitval

#define LINK(table, decl)                                                      \
  Namespace *current = namespaces.back();                                      \
  if (!current->names.contains(name.str)) {                                    \
    throw error_at(name, fmt::format("Internal parse error, aborting..."));    \
    abort();                                                                   \
  }                                                                            \
  current->table[name.str] = decl;

TypeId Parser::reserve_new_type(Token &name) {
  Namespace *current = namespaces.back();
  if (current->names.contains(name.str)) {
    throw error_at(
        name, fmt::format("Redefinition of previous name \'{}\'.", name.str));
  }
  int type_id = types.size();
  types.push_back(
      {static_cast<StructDecl *>(nullptr), static_cast<int>(type_id)});
  current->names.insert(name.str);
  current->type_decls[name.str] = type_id;
  return type_id;
}

void Parser::link_type(Token &name, TypeDeclPtr type_decl_ptr) {
  Namespace *current = namespaces.back();
  if (!current->names.contains(name.str)) {
    throw error_at(name, fmt::format("Internal parse error, aborting..."));
    abort();
  }
  types[current->type_decls[name.str]].type_decl_ptr = type_decl_ptr;
}

void Parser::reserve_fun(Token &name) {
  RESERVE(fun_decls, static_cast<FunDecl *>(nullptr));
}

void Parser::link_fun(Token &name, FunDecl *fun_decl) {
  LINK(fun_decls, fun_decl)
}
void Parser::reserve_var(Token &name) {
  RESERVE(var_decls, static_cast<VarDecl *>(nullptr));
}

void Parser::link_var(Token &name, VarDecl *var_decl) {
  LINK(var_decls, var_decl)
}

#undef RESERVE
#undef LINK

void Parser::add_type(Token &name, TypeId id) {
  Namespace *current = namespaces.back();
  if (current->names.contains(name.str)) {
    throw error_at(
        name, fmt::format("Redefinition of previous name \'{}\'.", name.str));
  }
  current->names.insert(name.str);
  current->type_decls[name.str] = id;
}
void Parser::add_fun(Token &name, FunDecl *fun_decl) {
  Namespace *current = namespaces.back();
  if (current->names.contains(name.str)) {
    throw error_at(
        name, fmt::format("Redefinition of previous name \'{}\'.", name.str));
  }
  current->names.insert(name.str);
  current->fun_decls[name.str] = fun_decl;
}
void Parser::add_var(Token &name, VarDecl *var_decl) {
  Namespace *current = namespaces.back();
  if (current->names.contains(name.str)) {
    throw error_at(
        name, fmt::format("Redefinition of previous name \'{}\'.", name.str));
  }
  current->names.insert(name.str);
  current->var_decls[name.str] = var_decl;
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
  if (is_at_end()) {
    return false;
  }
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

GenType Parser::type_name() {
  Token name = expect(IDENTIFIER, "Expected type name.");
  std::vector<GenType> params;
  if (match(LEFT_BRACKET)) {
    do {
      params.push_back(type_name());
    } while (match(COMMA));
    expect(RIGHT_BRACKET, "Expect ']' after type arguments.");
  }
  return GenType{std::string(name.str), params};
}

std::pair<Token, GenType> Parser::ident_type() {
  std::pair<Token, GenType> res;
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

std::unique_ptr<StructDecl> Parser::struct_declaration() {
  Token name = expect(IDENTIFIER, "Expected struct name.");
  expect(LEFT_BRACE, "Expect '{' before struct body.");

  std::vector<std::pair<Token, GenType>> fields;
  std::vector<std::unique_ptr<FunDecl>> methods;
  Namespace namesp;
  namespaces.push_back(&namesp);
  TypeId id = reserve_new_type(name);

  std::unordered_set<std::string_view> field_names;
  try {
    while (!check(RIGHT_BRACE) && !is_at_end()) {
      if (match(FUN)) {
        methods.emplace_back(function_declaration());
        add_fun(methods.back()->name, methods.back().get());
      } else if (match(SEMICOLON)) {
        // skip
      } else {
        auto field_decl = ident_type();
        if (field_names.contains(field_decl.first.str)) {
          throw error_at(field_decl.first, "Redefinition of field name");
        }
        field_names.insert(field_decl.first.str);
        fields.emplace_back(field_decl);
      }
    }
  } catch (ParseError &err) {
    namespaces.pop_back();
    throw err;
  }

  auto res =
      std::make_unique<StructDecl>(name, fields, std::move(methods), namesp);
  link_type(name, res.get());

  namespaces.pop_back();
  add_type(name, id);

  expect(RIGHT_BRACE, "Expect '}' after struct body");
  return res;
}

std::unique_ptr<EnumDecl> Parser::enum_declaration() {
  Token name = expect(IDENTIFIER, "Expected enum name.");
  expect(LEFT_BRACE, "Expect '{' before enum body.");

  std::vector<std::pair<Token, GenType>> variants;
  std::vector<std::unique_ptr<FunDecl>> methods;
  Namespace namesp;
  namespaces.push_back(&namesp);
  TypeId id = reserve_new_type(name);

  std::unordered_set<std::string_view> variant_names;
  try {
    while (!check(RIGHT_BRACE) && !is_at_end()) {
      if (match(FUN)) {
        methods.emplace_back(function_declaration());
        add_fun(methods.back()->name, methods.back().get());
      } else if (match(SEMICOLON)) {
        // continue
      } else {
        auto variant_decl = ident_type();
        if (variant_names.contains(variant_decl.first.str)) {
          throw error_at(variant_decl.first, "Redefinition of variant name");
        }
        variant_names.insert(variant_decl.first.str);
        variants.emplace_back(variant_decl);
      }
    }
  } catch (ParseError &err) {
    namespaces.pop_back();
    throw err;
  }

  auto res =
      std::make_unique<EnumDecl>(name, variants, std::move(methods), namesp);
  link_type(name, res.get());

  namespaces.pop_back();
  add_type(name, id);

  expect(RIGHT_BRACE, "Expect '}' after enum body.");
  return res;
}

std::unique_ptr<FunDecl> Parser::function_declaration() {
  Token name = expect(IDENTIFIER, "Expected function name.");
  expect(LEFT_PAREN, "Expect '(' after function name.");

  Namespace namesp;
  namespaces.push_back(&namesp);
  // function can refer to itself in body
  reserve_fun(name);

  std::vector<std::unique_ptr<VarDecl>> parameters;
  std::unique_ptr<Block> body(nullptr);
  try {
    if (!check(RIGHT_PAREN)) {
      do {
        parameters.push_back(variable_declaration());
        if (!parameters.back()->type_specifier.has_value()) {
          throw error_at(
              parameters.back()->name, "Function parameter must be typed.");
        }
        if (!parameters.back()->initializer.is<std::monostate>()) {
          throw error_at(parameters.back()->name,
              "Function parameter cannot (currently) have initializer.");
        }
      } while (match(COMMA));
    }
    expect(RIGHT_PAREN, "Expect ')' after parameters.");
    expect(LEFT_BRACE, "Expect '{' before function body.");
    body = block(namesp);
  } catch (ParseError &err) {
    namespaces.pop_back();
    throw err;
  }
  namespaces.pop_back();
  // check for return type
  return std::make_unique<FunDecl>(
      name, std::move(parameters), std::move(body));
}

std::unique_ptr<VarDecl> Parser::variable_declaration() {
  Token name = expect(IDENTIFIER, "Expected identifier.");
  std::optional<GenType> type;
  Expr initializer = ExprVariant{std::monostate{}};
  if (!check(EQUAL)) {
    type = type_name();
  }
  if (match(EQUAL)) {
    initializer = Expr{expression_bp(0)};
  }
  expect(SEMICOLON, "Expect ';' (EOL) after variable declaration.");
  auto res = std::make_unique<VarDecl>(name, type, std::move(initializer));
  add_var(name, res.get());
  return res;
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
    } else if (match(FOR)) {
    } else {
      return expression_statement();
    }
  } catch (ParseError &err) {
    synchronize();
  }
  return Stmt{std::monostate{}};
}

std::unique_ptr<Block> Parser::block() {
  std::vector<Stmt> res;
  Namespace namesp;
  namespaces.push_back(&namesp);
  try {
    while (!check(RIGHT_BRACE) && !is_at_end()) {
      Stmt stmt = statement();
      if (!stmt.is<std::monostate>()) {
        res.emplace_back(std::move(stmt));
      }
    }
    expect(RIGHT_BRACE, "Expected '}' after block.");
  } catch (ParseError &err) {
    namespaces.pop_back();
  }
  namespaces.pop_back();
  return std::make_unique<Block>(std::move(res), namesp);
}

std::unique_ptr<Block> Parser::block(Namespace &namesp) {
  std::vector<Stmt> res;
  while (!check(RIGHT_BRACE) && !is_at_end()) {
    Stmt stmt = statement();
    if (!stmt.is<std::monostate>()) {
      res.emplace_back(std::move(stmt));
    }
  }
  expect(RIGHT_BRACE, "Expected '}' after block.");
  return std::make_unique<Block>(std::move(res), namesp);
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
    return Stmt{std::make_unique<Assign>(
        op, Expr{std::move(expr)}, Expr{std::move(rhs)})};
  }
  default:
    break;
  }
  expect(SEMICOLON, "Expect ';' (EOL) after expression.");
  return Stmt{std::make_unique<Expression>(std::move(expr))};
}

// parses expression above minimum binding power argument
// only consume tokens that are in expression!
Expr Parser::expression_bp(int min_bp) {
  Token token = advance();
  std::optional<Expr> lhs = std::nullopt;
  // parse LHS of expression
  if (prefix_binding_power(token.lexeme) != -1) {
    lhs = Expr{std::make_unique<Unary>(to_unaryop(token.lexeme),
        Expr(expression_bp(prefix_binding_power(token.lexeme))))};
  }
  switch (token.lexeme) {
    // clang-format off
    case TRUE: lhs = Expr{std::make_unique<Literal>(true)}; break;
    case FALSE: lhs = Expr{std::make_unique<Literal>(false)}; break;
    case INTEGER: lhs = Expr{std::make_unique<Literal>(std::stoi(std::string(token.str)))}; break;
    case DECIMAL: lhs = Expr{std::make_unique<Literal>(std::stod(std::string(token.str)))}; break;
    case STRING: lhs = Expr{std::make_unique<Literal>(std::string{token.str})}; break;
    case CHARACTER: lhs = Expr{std::make_unique<Literal>(token.str[1])}; break; // fix later
    case IDENTIFIER: lhs = Expr{std::make_unique<Variable>(token)}; break;
    case IF: {
      std::vector<std::unique_ptr<If::Branch>> branches;
      Expr condition = expression_bp(0);
      expect(LEFT_BRACE, "Expected '{' after 'if'.");
      branches.emplace_back(std::make_unique<If::Branch>(Expr(std::move(condition)), block()));

      while (match(ELSE)) {
        if (match(IF)) {
          branches.emplace_back(std::make_unique<If::Branch>(Expr(expression_bp(0)), block()));
        }
        else {
          expect(LEFT_BRACE, "Expected '{' after 'else'.");
          branches.emplace_back(std::make_unique<If::Branch>(Expr(std::unique_ptr<Literal>(nullptr)), block()));
          break;
        }
      }
      lhs = Expr{std::make_unique<If>(std::move(branches))};
      break;
    }
    case ELSE: {
      throw error_prev("Unexpected 'else' with no preceding 'if'.");
    }
    case LEFT_BRACE: {
      lhs = Expr{block()};
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
        lhs = Expr{std::make_unique<FunCall>(
            Expr{std::move(lhs.value())}, std::move(args))};
        break;
      }
      case DOT: {
        advance();
        Token ident = expect(IDENTIFIER, "Expected identifier after '.'.");
        lhs =
            Expr{std::make_unique<DotRef>(Expr{std::move(lhs.value())}, ident)};
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
      lhs = Expr{std::make_unique<Binary>(
          op, Expr{std::move(lhs.value())}, Expr{std::move(rhs)})};
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
    }
  }
  return AST(std::move(decls), globals);
}

} // namespace cinnabar
