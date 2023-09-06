#include "parser.hpp"

#include "fmt/core.h"

#include <unordered_set>

namespace cinnabar {

using enum Lexeme;

Parser::Parser(std::string source) : lexer(std::move(source)) {
  lexer.lex();
  tokens = lexer.get_tokens();
}

template <typename T> void Parser::reserve_name(const Token &name) {
  Namespace *current = namespaces.back();
  if (current->names.contains(name.str)) {
    throw error_at(
        name, fmt::format("Redefinition of previous name \'{}\'.", name.str)
    );
  }
  current->names[name.str] = static_cast<T>(nullptr);
}

template <typename T> void Parser::link_name(const Token &name, T decl) {
  Namespace *current = namespaces.back();
  if (!current->names.contains(name.str) ||
      current->names[name.str] != DeclPtr(static_cast<T>(nullptr))) {
    throw error_at(
        name, fmt::format("Internal error with linking name {}.", name.str)
    );
  }
  current->names[name.str] = decl;
}

template <typename T> void Parser::add_name(const Token &name, T decl) {
  Namespace *current = namespaces.back();
  if (current->names.contains(name.str)) {
    throw error_at(
        name, fmt::format("Redefinition of previous name \'{}\'.", name.str)
    );
  }
  current->names[name.str] = DeclPtr{decl};
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

Parser::ParseError Parser::error_at(const Token &t, std::string_view message) {
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

GenericInst Parser::type_name() {
  Token name = expect(IDENTIFIER, "Expected type name.");
  std::vector<GenericInst> params;
  if (match(LEFT_BRACKET)) {
    if (name.str == "Array") {
      params.push_back(type_name());
      match(COMMA);
      params.push_back(GenericInst{
          expect(INTEGER, "Expect integer parameter for Array[T, N].").str});
    } else {
      do {
        params.push_back(type_name());
      } while (match(COMMA));
    }
    expect(RIGHT_BRACKET, "Expect ']' after type arguments.");
  }
  return GenericInst{std::string(name.str), params};
}

TypedName Parser::ident_type() {
  TypedName res(Token(), GenericInst{""});
  res.name = expect(IDENTIFIER, "Expected identifier.");
  res.gentype = type_name();
  return res;
}

Stmt Parser::toplevel_declaration() {
  try {
    if (match(STRUCT)) {
      return Stmt{Declaration{struct_declaration()}};
    }
    if (match(ENUM)) {
      return Stmt{Declaration{enum_declaration()}};
    }
    if (match(FUN)) {
      return Stmt{Declaration{function_declaration()}};
    }
    if (match({LET, VAR})) {
      return Stmt{Declaration{variable_declaration()}};
    }
    if (match(SEMICOLON)) {
      return Stmt{std::monostate{}};
    }
    throw error_cur("Expected top-level declaration.");
  } catch (ParseError &err) {
    synchronize();
  }
  return Stmt{std::monostate{}};
}

std::unique_ptr<StructDecl> Parser::struct_declaration() {
  Token name = expect(IDENTIFIER, "Expected struct name.");
  expect(LEFT_BRACE, "Expect '{' before struct body.");

  std::vector<TypedName> fields;
  std::vector<std::unique_ptr<FunDecl>> methods;
  auto namesp = std::make_unique<Namespace>(namespaces.back());
  namespaces.push_back(namesp.get());
  reserve_name<StructDecl *>(name);

  std::unordered_set<std::string_view> member_names;
  try {
    while (!check(RIGHT_BRACE) && !is_at_end()) {
      if (match(FUN)) {
        methods.emplace_back(function_declaration());
        if (member_names.contains(methods.back()->name.str)) {
          throw error_at(methods.back()->name, "Redefinition of member name");
        }
        member_names.insert(methods.back()->name.str);
      } else if (match(SEMICOLON)) {
        // skip
      } else {
        auto field_decl = ident_type();
        if (member_names.contains(field_decl.name.str)) {
          throw error_at(field_decl.name, "Redefinition of member name");
        }
        member_names.insert(field_decl.name.str);
        fields.emplace_back(field_decl);
      }
    }
  } catch (ParseError &err) {
    namespaces.pop_back();
    throw err;
  }

  auto res = std::make_unique<StructDecl>(
      name, GenericName{std::string(name.str), {}}, fields, std::move(methods),
      std::move(namesp)
  );
  link_name<StructDecl *>(name, res.get());

  namespaces.pop_back();
  add_name<StructDecl *>(name, res.get());

  for (auto &fun_ptr : res->methods) {
    fun_ptr->method_of = res.get();
  }

  expect(RIGHT_BRACE, "Expect '}' after struct body");
  return res;
}

std::unique_ptr<EnumDecl> Parser::enum_declaration() {
  Token name = expect(IDENTIFIER, "Expected enum name.");
  expect(LEFT_BRACE, "Expect '{' before enum body.");

  std::vector<TypedName> variants;
  std::vector<std::unique_ptr<FunDecl>> methods;
  auto namesp = std::make_unique<Namespace>(namespaces.back());
  namespaces.push_back(namesp.get());
  reserve_name<EnumDecl *>(name);

  std::unordered_set<std::string_view> member_names;
  try {
    while (!check(RIGHT_BRACE) && !is_at_end()) {
      if (match(FUN)) {
        methods.emplace_back(function_declaration());
        if (member_names.contains(methods.back()->name.str)) {
          throw error_at(methods.back()->name, "Redefinition of member name");
        }
        member_names.insert(methods.back()->name.str);
      } else if (match(SEMICOLON)) {
        // continue
      } else {
        TypedName variant{
            expect(IDENTIFIER, "Expected identifier."), GenericInst{"unit"}};
        if (!check(SEMICOLON)) {
          variant.gentype = type_name();
        }
        if (member_names.contains(variant.name.str)) {
          throw error_at(variant.name, "Redefinition of member name");
        }
        member_names.insert(variant.name.str);
        variants.emplace_back(variant);
      }
    }
  } catch (ParseError &err) {
    namespaces.pop_back();
    throw err;
  }

  auto res = std::make_unique<EnumDecl>(
      name, GenericName{std::string(name.str), {}}, variants,
      std::move(methods), std::move(namesp)
  );
  link_name<EnumDecl *>(name, res.get());

  namespaces.pop_back();
  add_name<EnumDecl *>(name, res.get());

  for (auto &fun_ptr : res->methods) {
    fun_ptr->method_of = res.get();
  }

  expect(RIGHT_BRACE, "Expect '}' after enum body.");
  return res;
}

std::unique_ptr<FunDecl> Parser::function_declaration() {
  Token name = expect(IDENTIFIER, "Expected function name.");
  expect(LEFT_PAREN, "Expect '(' after function name.");

  auto namesp = std::make_unique<Namespace>(namespaces.back());
  namespaces.push_back(namesp.get());
  // function can refer to itself in body
  reserve_name<FunDecl *>(name);

  std::vector<std::unique_ptr<VarDecl>> parameters;
  GenericInst return_type("unit");
  std::unique_ptr<Block> body(nullptr);

  try {
    if (!check(RIGHT_PAREN)) {
      do {
        Token name =
            expect(IDENTIFIER, "Expected identifier for function parameter");
        if (!check(IDENTIFIER)) {
          error_cur("Function parameter must have type specifier");
        }
        auto type = type_name();
        auto param = std::make_unique<VarDecl>(name, type, std::nullopt);
        add_name<VarDecl *>(name, param.get());
        parameters.push_back(std::move(param));
      } while (match(COMMA));
    }
    expect(RIGHT_PAREN, "Expect ')' after parameters.");

    if (!check(LEFT_BRACE)) {
      return_type = type_name();
    }
    expect(LEFT_BRACE, "Expect '{' before function body.");
    body = block(std::move(namesp));
  } catch (ParseError &err) {
    namespaces.pop_back();
    throw err;
  }
  auto res = std::make_unique<FunDecl>(
      name, GenericName{std::string(name.str), {}}, std::move(parameters),
      return_type, std::move(body)
  );
  link_name<FunDecl *>(name, res.get());

  namespaces.pop_back();
  add_name<FunDecl *>(name, res.get());
  // check for return type
  return res;
}

std::unique_ptr<VarDecl> Parser::variable_declaration() {
  Token name = expect(IDENTIFIER, "Expected identifier.");
  std::optional<GenericInst> type;
  std::optional<Expr> initializer = std::nullopt;
  if (!check(EQUAL)) {
    type = type_name();
  }
  if (match(EQUAL)) {
    initializer = Expr{expression_bp(0)};
  }
  expect(SEMICOLON, "Expect ';' (EOL) after variable declaration.");
  auto res = std::make_unique<VarDecl>(name, type, std::move(initializer));
  add_name<VarDecl *>(name, res.get());
  return res;
}

Stmt Parser::statement() {
  try {
    if (match(STRUCT)) {
      return Stmt{Declaration{struct_declaration()}};
    }
    if (match(ENUM)) {
      return Stmt{Declaration{enum_declaration()}};
    }
    if (match(FUN)) {
      return Stmt{Declaration{function_declaration()}};
    }
    if (match({LET, VAR})) {
      return Stmt{Declaration{variable_declaration()}};
    }
    if (match(SEMICOLON)) {
      return Stmt{std::monostate{}};
    }
    if (match(WHILE)) {
      error_prev("Currently does not support while loops.");
    }
    if (match(FOR)) {
      error_prev("Currently does not support for loops.");
    }
    if (match(RETURN)) {
      if (match(SEMICOLON)) {
        return Stmt{std::make_unique<Return>(std::nullopt)};
      }
      return Stmt{std::make_unique<Return>(expression_bp(0))};
    }
    return expression_statement();
  } catch (ParseError &err) {
    synchronize();
  }
  return Stmt{std::monostate{}};
}

std::unique_ptr<Block> Parser::block() {
  std::vector<Stmt> res;
  auto namesp = std::make_unique<Namespace>(namespaces.back());
  namespaces.push_back(namesp.get());
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
  return std::make_unique<Block>(std::move(res), std::move(namesp));
}

std::unique_ptr<Block> Parser::block(std::unique_ptr<Namespace> namesp) {
  std::vector<Stmt> res;
  while (!check(RIGHT_BRACE) && !is_at_end()) {
    Stmt stmt = statement();
    if (!stmt.is<std::monostate>()) {
      res.emplace_back(std::move(stmt));
    }
  }
  expect(RIGHT_BRACE, "Expected '}' after block.");
  return std::make_unique<Block>(std::move(res), std::move(namesp));
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
        op, Expr{std::move(expr)}, Expr{std::move(rhs)}
    )};
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
  if (const int prefix_bp = prefix_binding_power(token.lexeme);
      prefix_bp != -1) {
    lhs = Expr{std::make_unique<Unary>(
        to_unaryop(token.lexeme), expression_bp(prefix_bp)
    )};
  } else {
    lhs = expression_lhs(token);
  }
  // LHS done, parse RHS

  // parse postfix or infix with min_bp or greater
  while (cur_token().lexeme != END_OF_FILE) {
    token = cur_token();
    if (int postfix_bp = postfix_binding_power(token.lexeme);
        postfix_bp != -1) {
      // is a postfix op
      if (postfix_bp < min_bp) { // not enough binding power
        break;
      }
      switch (token.lexeme) {
      case LEFT_PAREN: {
        advance();
        std::vector<Expr> args = argument_list();
        auto *var = std::get_if<std::unique_ptr<Variable>>(&lhs.value().node);
        if (var != nullptr && (*var)->name.str == "__ref") {
          if (args.size() != 1) {
            error_prev("__ref can only be called on one argument.");
          }
          lhs = Expr{
              std::make_unique<Unary>(UnaryOp::REF, Expr{std::move(args[0])})};
        } else if (var != nullptr && (*var)->name.str == "__deref") {
          if (args.size() != 1) {
            error_prev("__deref can only be called on one argument.");
          }
          lhs = Expr{std::make_unique<Unary>(
              UnaryOp::DEREF, Expr{std::move(args[0])}
          )};
        } else {
          lhs = Expr{std::make_unique<FunCall>(
              Expr{std::move(lhs.value())}, std::move(args)
          )};
        }
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
    if (int infix_bp = infix_binding_power(token.lexeme); infix_bp != -1) {
      // is an infix op
      if (infix_bp < min_bp) {
        break;
      }
      advance();
      BinaryOp op = to_binop(token.lexeme);
      // recursive call with slightly higher binding power for
      // left-associativity
      auto rhs = expression_bp(infix_bp + 1);
      // if assign was an Expr, would do switch here
      lhs = Expr{std::make_unique<Binary>(
          op, Expr{std::move(lhs.value())}, Expr{std::move(rhs)}
      )};
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

Expr Parser::expression_lhs(const Token &token) {
  switch (token.lexeme) {
  // clang-format off
    case TRUE: return Expr{std::make_unique<Literal>(true)};
    case FALSE: return Expr{std::make_unique<Literal>(false)};
    case INTEGER: return Expr{std::make_unique<Literal>(std::stoi(std::string(token.str)))};
    case DECIMAL: return Expr{std::make_unique<Literal>(std::stod(std::string(token.str)))};
    case STRING: return Expr{std::make_unique<Literal>(std::string{token.str})};
    case CHARACTER: return Expr{std::make_unique<Literal>(token.str[1])}; // fix later
    case IDENTIFIER: return Expr{std::make_unique<Variable>(token)};
    case IF: {
      std::vector<std::unique_ptr<If::Branch>> branches;
      Expr condition = expression_bp(0);
      expect(LEFT_BRACE, "Expected '{' after condition.");
      branches.emplace_back(std::make_unique<If::Branch>(std::move(condition), block()));

      while (match(ELSE)) {
        if (match(IF)) {
          Expr condition = expression_bp(0);
          expect(LEFT_BRACE, "Expected '{' after condition.");
          branches.emplace_back(std::make_unique<If::Branch>(std::move(condition), block()));
        }
        else {
          expect(LEFT_BRACE, "Expected '{' after 'else'.");
          branches.emplace_back(std::make_unique<If::Branch>(Expr{std::make_unique<Literal>(true)}, block()));
          break;
        }
      }
      return Expr{std::make_unique<If>(std::move(branches))};
    }
    case ELSE: throw error_prev("Unexpected 'else' with no preceding 'if'.");
    case LEFT_BRACE: return Expr{block()};
    case LEFT_PAREN: {
      auto lhs = expression_bp(0);
      expect(RIGHT_PAREN, "Expected ')' following open '('.");
      return lhs;
    }
    default: throw error_prev(fmt::format("Unexpected token in expression: {}.", to_string(token.lexeme)));
    // clang-format on
  }
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
  auto globals = std::make_unique<Namespace>(nullptr);
  namespaces.push_back(globals.get());

  std::vector<std::unique_ptr<Primitive>> primitives;
  std::vector<std::unique_ptr<BuiltinType>> builtin_types;

  for (auto name : default_primitives) {
    primitives.push_back(std::make_unique<Primitive>(
        Token::make_builtin(name, Lexeme::IDENTIFIER)
    ));
    add_name<Primitive *>(primitives.back()->name, primitives.back().get());
  }

  // if (globals.names.contains(name)) {
  //   fmt::print(stderr, "Globals contained name '{}' matching builtin.\n",
  // name); abort();
  // }

  // struct Ref[T] {
  //   T *ptr;
  // }
  // clang-format off
  builtin_types.emplace_back(std::make_unique<BuiltinType>(
      Token::make_builtin("Ref", Lexeme::IDENTIFIER), GenericName{"Ref", {"T"}}
  ));
  add_name<BuiltinType *>(builtin_types.back()->name, builtin_types.back().get());

  builtin_types.emplace_back(std::make_unique<BuiltinType>(
      Token::make_builtin("VarRef", Lexeme::IDENTIFIER), GenericName{"VarRef", {"T"}}
  ));
  add_name<BuiltinType *>(builtin_types.back()->name, builtin_types.back().get());

  builtin_types.emplace_back(std::make_unique<BuiltinType>(
      Token::make_builtin("Span", Lexeme::IDENTIFIER), GenericName{"Span", {"T"}}
  ));
  add_name<BuiltinType *>(builtin_types.back()->name, builtin_types.back().get());

  builtin_types.emplace_back(std::make_unique<BuiltinType>(
      Token::make_builtin("VarSpan", Lexeme::IDENTIFIER), GenericName{"VarSpan", {"T"}}
  ));
  add_name<BuiltinType *>(builtin_types.back()->name, builtin_types.back().get());

  builtin_types.emplace_back(std::make_unique<BuiltinType>(
      Token::make_builtin("Array", Lexeme::IDENTIFIER), GenericName{"Array", {"T", "N"}}
  ));
  add_name<BuiltinType *>(builtin_types.back()->name, builtin_types.back().get());
  // clang-format on

  while (!is_at_end()) {
    Stmt decl = toplevel_declaration();
    if (!decl.is<std::monostate>()) {
      decls.emplace_back(std::move(decl.as<Declaration>()));
    }
  }

  return AST(
      std::move(decls), std::move(primitives), std::move(builtin_types),
      std::move(globals)
  );
}

} // namespace cinnabar
