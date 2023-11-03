#include "lexer.hpp"

#include <unordered_map>

namespace cinnabar {

using enum Lexeme;

// clang-format off
std::unordered_map<std::string_view, Lexeme> keywords = {
    {"as",       AS},
    {"break",    BREAK},
    {"const",    CONST},
    {"continue", CONTINUE},
    {"enum",     ENUM},
    {"else",     ELSE},
    {"false",    FALSE},
    {"fun",      FUN},
    {"for",      FOR},
    {"is",       IS},
    {"if",       IF},
    {"let",      LET},
    {"match",    MATCH},
    {"return",   RETURN},
    {"self",     SELF},
    {"struct",   STRUCT},
    {"true",     TRUE},
    {"type",     TYPE},
    {"var",      VAR},
    {"while",    WHILE},
    // clang-format on
};

Lexer::Lexer(std::string source) : source(source) {}

bool Lexer::is_at_end() {
  return static_cast<size_t>(cur_index) >= source.size();
}

char Lexer::advance() {
  col++;
  return source[cur_index++];
}
bool Lexer::match_advance(char expect) {
  if (is_at_end() || source[cur_index] != expect) {
    return false;
  }
  col++;
  advance();
  return true;
}

char Lexer::cur_char() { return is_at_end() ? '\0' : source[cur_index]; }

char Lexer::next_char() {
  return static_cast<size_t>(cur_index) + 1 >= source.size()
             ? '\0'
             : source[cur_index + 1];
}

Token &Lexer::prev_token() { return tokens.back(); }

void Lexer::pop_added_semicolons() {
  while (!tokens.empty() && tokens.back().lexeme == SEMICOLON &&
         tokens.back().str == "\n") {
    tokens.pop_back();
  }
}

void Lexer::read_character() {
  if (is_at_end()) {
    error("Unterminated character literal.");
  }
  char literal = advance();
  if (literal == '\'') {
    error("Expected character in character literal.");
  } else if (literal == '\\') {
    if (is_at_end()) {
      error("Escape sequence terminated with end of file.");
    }
    char escape = advance();
    // unfinished
    switch (escape) {
    case '0':
    case '\'':
    case '\"':
    case '\\':
    case 't':
    case 'n':
      break;
    }
  }
  if (is_at_end() || !match_advance('\'')) {
    error("Expected \' after character literal.");
  }

  add_token(CHARACTER);
}

// reads string starting from character after opening "
void Lexer::read_string() {
  if (is_at_end()) {
    error("Unterminated string");
  }
  for (char c = advance(); c != '"'; c = advance()) {
    switch (c) {
      case '\\':
        c = advance();
        switch (c) {
          case '0':
          case 't':
          case 'n':
          case '\\':
          case '\'':
          case '"':
            break;
          default:
            error(fmt::format("Unrecognized escape character '\\{}'", cur_char()));
        }
      break;
      case '\n':
        error("Cannot have newlines in string literal");
      break;
      default:
      break;
    }
    if (is_at_end()) {
      error("Unterminated string");
    }
  }
  //advance(); // closing "
  add_token(STRING);
}

void Lexer::read_number() {
  while (std::isdigit(cur_char())) {
    advance();
  }
  if (cur_char() == '.' && isdigit(next_char())) {
    advance();
    while (std::isdigit(cur_char())) {
      advance();
    }
    add_token(DECIMAL);
    return;
  }
  add_token(INTEGER);
}

void Lexer::read_identifier() {
  while (std::isalnum(cur_char()) || cur_char() == '_') {
    advance();
  }
  std::string_view text = srcsubstr(start_index, cur_index - 1);
  Lexeme l = keywords.contains(text) ? keywords[text] : IDENTIFIER;
  if (l == ELSE) { // pop auto added semicolons
    pop_added_semicolons();
  }
  add_token(l);
}

std::string_view Lexer::srcsubstr(int start, int end) {
  return std::string_view(source).substr(start, end - start + 1);
}

void Lexer::add_token(Lexeme l) {
  std::string_view strv = srcsubstr(start_index, cur_index - 1);
  tokens.emplace_back(strv, l, line, col - strv.size());
}

void Lexer::scan_token() {
  char c = advance();
  // clang-format off
  switch (c) {
  case '(': add_token(LEFT_PAREN); break;
  case ')': add_token(RIGHT_PAREN); break;
  case '[': add_token(LEFT_BRACKET); break;
  case ']': add_token(RIGHT_BRACKET); break;
  case '{': add_token(LEFT_BRACE); break;
  case '}': {
    add_token(SEMICOLON); // if true { a; }
    add_token(RIGHT_BRACE); break;
  }
  case ',': add_token(COMMA); break;
  case ':': add_token(COLON); break;
  case ';': add_token(SEMICOLON); break;
  case '@': add_token(AT); break;
  case '$': add_token(DOLLAR); break;
  case '?': add_token(QUESTION); break;
  case '.': add_token(match_advance('.') ? DOT_DOT : DOT); break;
  case '+': add_token(match_advance('=') ? PLUS_EQUAL : PLUS); break;
  case '-': add_token(match_advance('=') ? MINUS_EQUAL : MINUS); break;
  case '*': add_token(match_advance('=') ? STAR_EQUAL : STAR); break;
  case '/': {
    if (match_advance('/')) {
      while (cur_char() != '\n' && !is_at_end())
        advance();
    }
    else if (match_advance('*')) { // nested comments /* /* */ */
      size_t stack = 1;
      while (!is_at_end() && stack > 0) {
        if (match_advance('/') && match_advance('*')) stack++;
        else if (match_advance('*') && match_advance('/')) stack--;
        else if (match_advance('\n')) line++;
        else advance();
      }
    }
    else
      add_token(match_advance('=') ? SLASH_EQUAL : SLASH);
    break;
  }
  case '%': add_token(match_advance('=') ? MODULO_EQUAL : MODULO); break;
  case '!': add_token(match_advance('=') ? BANG_EQUAL : BANG); break;
  case '=': add_token(match_advance('=') ? EQUAL_EQUAL : EQUAL); break;
  case '^': add_token(match_advance('=') ? CARET_EQUAL : CARET); break;
  case '~': add_token(TILDE); break;
  case '&':
    if (match_advance('&')) add_token(AMPERSAND_AMPERSAND);
    else add_token(match_advance('=') ? AMPERSAND_EQUAL : AMPERSAND);
    break;
  case '|':
    if (match_advance('|')) add_token(BAR_BAR);
    else add_token(match_advance('=') ? BAR_EQUAL : BAR);
    break;
  case '>':
    if (match_advance('>')) add_token(match_advance('=') ? GREATER_GREATER_EQUAL : GREATER_GREATER);
    else add_token(match_advance('=') ? GREATER_EQUAL : GREATER);
    break;
  case '<':
    if (match_advance('<')) add_token(match_advance('=') ? LESS_LESS_EQUAL : LESS_LESS);
    else add_token(match_advance('=') ? LESS_EQUAL : LESS);
    break;
  case ' ':
  case '\r':
  case '\t': break;
  case '\n': {
    if (!tokens.empty()) {
      // add more cases later
      switch (prev_token().lexeme) {
        case LEFT_BRACE:
        case SEMICOLON:
          break;
        default:
          add_token(SEMICOLON);
          break;
      }
    }
    line++;
    col = 1;
    break;
  }
  case '\'': {
    read_character(); break;
  }
  case '"': read_string(); break;
  default:
    if (isdigit(c)) {
      read_number();
    } else if (isalpha(c) || c == '_')
      read_identifier();
    else
      error(fmt::format("Unexpected character '{}'", c));
  }
  // clang-format on
}

bool Lexer::lex() {
  while (!is_at_end()) {
    start_index = cur_index;
    scan_token();
  }
  add_token(END_OF_FILE);
  return !has_error;
}

void Lexer::error(std::string_view message) {
  fmt::print("Error at line {}, column {}:\n\t{}\n", line, col, message);
}

std::span<Token> Lexer::get_tokens() { return std::span<Token>(tokens); }

std::string Lexer::token_repr() {
  std::string res = fmt::format("Lexed Tokens\n============\n");
  if (!tokens.empty()) {
    res += fmt::format("Line {}: ", tokens[0].line);
  }
  for (size_t i = 0; i < tokens.size(); i++) {
    Token &token = tokens[i];
    res += fmt::format(
        "{}[{}]{} ", to_string(token.lexeme),
        token.str == "\n" ? "\\n" : token.str, token.col
    );
    if (i != tokens.size() - 1 && tokens[i + 1].line != token.line) {
      res += fmt::format("\nLine {}: ", tokens[i + 1].line);
    }
  }
  res += "\n============\n";
  return res;
}

} // namespace cinnabar
