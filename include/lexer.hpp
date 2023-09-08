#pragma once

#include "common.hpp"

#include <span>
#include <string>
#include <vector>

namespace cinnabar {

enum class Lexeme : uint8_t {
  // single character
  LEFT_PAREN,            // (
  RIGHT_PAREN,           // )
  LEFT_BRACKET,          // [
  RIGHT_BRACKET,         // ]
  LEFT_BRACE,            // {
  RIGHT_BRACE,           // }
  COMMA,                 // ,
  COLON,                 // :
  SEMICOLON,             // ;
  AT,                    // @
  DOLLAR,                // $
  QUESTION,              // ?

  // one or two character
  DOT,                   // .
  DOT_DOT,               // ..
  PLUS,                  // +
  PLUS_EQUAL,            // +=
  MINUS,                 // -
  MINUS_EQUAL,           // -=
  STAR,                  // *
  STAR_EQUAL,            // *=
  SLASH,                 // /
  SLASH_EQUAL,           // /=
  MODULO,                // %
  MODULO_EQUAL,          // %=
  BANG,                  // !
  BANG_EQUAL,            // !=
  EQUAL,                 // =
  EQUAL_EQUAL,           // ==
  CARET,                 // ^
  CARET_EQUAL,           // ^=
  TILDE,                 // ~
  AMPERSAND,             // &
  AMPERSAND_AMPERSAND,   // &&
  AMPERSAND_EQUAL,       // &=
  BAR,                   // |
  BAR_BAR,               // ||
  BAR_EQUAL,             // |=
  GREATER,               // >
  GREATER_EQUAL,         // >=
  GREATER_GREATER,       // >>
  GREATER_GREATER_EQUAL, // >>=
  LESS,                  // <
  LESS_EQUAL,            // <=
  LESS_LESS,             // <<
  LESS_LESS_EQUAL,       // <<=

  // literals
  IDENTIFIER,
  CHARACTER,
  STRING,
  INTEGER,
  DECIMAL,
  TRUE,
  FALSE,
  
  // keywords
  AS,
  BREAK,
  CONST,
  CONTINUE,
  ENUM,
  ELSE,
  FUN,
  FOR,
  IF,
  IS,
  IN,
  LET,
  MATCH,
  RETURN,
  SELF,
  STRUCT,
  TYPE,
  VAR,
  WHILE,

  END_OF_FILE,
  NOT_A_TOKEN,
};

constexpr std::string_view to_string(Lexeme l) {
  switch (l) {
  case Lexeme::LEFT_PAREN:            return "LEFT_PAREN";
  case Lexeme::RIGHT_PAREN:           return "RIGHT_PAREN";
  case Lexeme::LEFT_BRACKET:          return "LEFT_BRACKET";
  case Lexeme::RIGHT_BRACKET:         return "RIGHT_BRACKET";
  case Lexeme::LEFT_BRACE:            return "LEFT_BRACE";
  case Lexeme::RIGHT_BRACE:           return "RIGHT_BRACE";
  case Lexeme::COMMA:                 return "COMMA";
  case Lexeme::COLON:                 return "COLON";
  case Lexeme::SEMICOLON:             return "SEMICOLON";
  case Lexeme::AT:                    return "AT";
  case Lexeme::DOLLAR:                return "DOLLAR";
  case Lexeme::QUESTION:              return "QUESTION";

  case Lexeme::DOT:                   return "DOT";
  case Lexeme::DOT_DOT:               return "DOT_DOT";
  case Lexeme::PLUS:                  return "PLUS";
  case Lexeme::PLUS_EQUAL:            return "PLUS_EQUAL";
  case Lexeme::MINUS:                 return "MINUS";
  case Lexeme::MINUS_EQUAL:           return "MINUS_EQUAL";
  case Lexeme::STAR:                  return "STAR";
  case Lexeme::STAR_EQUAL:            return "STAR_EQUAL";
  case Lexeme::SLASH:                 return "SLASH";
  case Lexeme::SLASH_EQUAL:           return "SLASH_EQUAL";
  case Lexeme::MODULO:                return "MODULO";
  case Lexeme::MODULO_EQUAL:          return "MODULO_EQUAL";
  case Lexeme::BANG:                  return "BANG";
  case Lexeme::BANG_EQUAL:            return "BANG_EQUAL";
  case Lexeme::EQUAL:                 return "EQUAL";
  case Lexeme::EQUAL_EQUAL:           return "EQUAL_EQUAL";
  case Lexeme::CARET:                 return "CARET";
  case Lexeme::CARET_EQUAL:           return "CARET_EQUAL";
  case Lexeme::TILDE:                 return "TILDE";
  case Lexeme::AMPERSAND:             return "AMPERSAND";
  case Lexeme::AMPERSAND_AMPERSAND:   return "AMPERSAND_AMPERSAND";
  case Lexeme::AMPERSAND_EQUAL:       return "AMPERSAND";
  case Lexeme::BAR:                   return "BAR";
  case Lexeme::BAR_BAR:               return "BAR_BAR";
  case Lexeme::BAR_EQUAL:             return "BAR_EQUAL";
  case Lexeme::GREATER:               return "GREATER";
  case Lexeme::GREATER_EQUAL:         return "GREATER_EQUAL";
  case Lexeme::GREATER_GREATER:       return "GREATER_GREATER";
  case Lexeme::GREATER_GREATER_EQUAL: return "GREATER_GREATER_EQUAL";
  case Lexeme::LESS:                  return "LESS";
  case Lexeme::LESS_EQUAL:            return "LESS_EQUAL";
  case Lexeme::LESS_LESS:             return "LESS_LESS";
  case Lexeme::LESS_LESS_EQUAL:       return "LESS_LESS_EQUAL";

  case Lexeme::IDENTIFIER:            return "IDENTIFIER";
  case Lexeme::CHARACTER:             return "CHARACTER";
  case Lexeme::STRING:                return "STRING";
  case Lexeme::INTEGER:               return "INTEGER";
  case Lexeme::DECIMAL:               return "DECIMAL";
  case Lexeme::TRUE:                  return "TRUE";
  case Lexeme::FALSE:                 return "FALSE";

  case Lexeme::AS:                    return "AS";
  case Lexeme::BREAK:                 return "BREAK";
  case Lexeme::CONST:                 return "CONST";
  case Lexeme::CONTINUE:              return "CONTINUE";
  case Lexeme::ENUM:                  return "ENUM";
  case Lexeme::ELSE:                  return "ELSE";
  case Lexeme::FUN:                   return "FUN";
  case Lexeme::FOR:                   return "FOR";
  case Lexeme::IS:                    return "IS";
  case Lexeme::IF:                    return "IF";
  case Lexeme::IN:                    return "IN";
  case Lexeme::LET:                   return "LET";
  case Lexeme::MATCH:                 return "MATCH";
  case Lexeme::RETURN:                return "RETURN";
  case Lexeme::SELF:                  return "SELF";
  case Lexeme::STRUCT:                return "STRUCT";
  case Lexeme::TYPE:                  return "TYPE";
  case Lexeme::VAR:                   return "VAR";
  case Lexeme::WHILE:                 return "WHILE";

  case Lexeme::END_OF_FILE:           return "END_OF_FILE";
  case Lexeme::NOT_A_TOKEN:           return "NOT_A_TOKEN";
  default:
    return "ERROR_TOKEN_NOT_FOUND";
  }
}

struct Token {
  std::string_view str;
  Lexeme lexeme;
  int line;
  int col;
  inline Token(): lexeme(Lexeme::NOT_A_TOKEN), line(-1), col(-1) {};
  inline Token(std::string_view str, Lexeme lexeme, int line, int col):
      str(str), lexeme(lexeme), line(line), col(col) { }

  static inline Token make_builtin(std::string_view str, Lexeme lexeme) {
    return {str, lexeme, -1, -1};
  }
};

class Lexer {
  std::string source;
  std::vector<Token> tokens;
  int start_index = 0;
  int cur_index = 0;
  int line = 1;
  int col = 1;
  bool has_error = false;
  bool is_at_end();
  // returns current char and advances 1
  char advance();
  // checks if current char is == expected, if == then advance
  bool match_advance(char expect);
  char cur_char();
  char next_char();
  
  // check for empty!
  Token &prev_token();
  
  void pop_added_semicolons();

  void read_character();
  void read_string();
  void read_number();
  void read_identifier();

  // string_view substr [start, end]
  std::string_view srcsubstr(int start, int end);
  void add_token(Lexeme l);

  void scan_token();
  void error(std::string_view message);

public:
  Lexer(std::string source);
  bool lex();
  std::span<Token> get_tokens();
  std::string token_repr();
};
}
