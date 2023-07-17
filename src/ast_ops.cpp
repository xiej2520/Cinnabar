#include "ast_ops.hpp"

namespace cinnabar {

using enum Lexeme;

UnaryOp to_unaryop(Lexeme l) {
  switch (l) {
  case PLUS:
    return UnaryOp::PLUS;
  case MINUS:
    return UnaryOp::NEG;
  case BANG:
    return UnaryOp::NOT;
  case AMPERSAND:
    return UnaryOp::REF; // ?
  case STAR:
    return UnaryOp::DEREF; // ?
  default:
    return UnaryOp::ERROR;
  }
}

BinaryOp to_binop(Lexeme l) {
  switch (l) {
  // clang-format off
  case PLUS:                return BinaryOp::ADD;
  case MINUS:               return BinaryOp::SUB;
  case STAR:                return BinaryOp::MUL;
  case SLASH:               return BinaryOp::DIV;
  case MODULO:              return BinaryOp::MOD;
  case EQUAL:               return BinaryOp::EQ;
  case BANG_EQUAL:          return BinaryOp::NEQ;
  case GREATER:             return BinaryOp::GT;
  case LESS:                return BinaryOp::LT;
  case GREATER_EQUAL:       return BinaryOp::GTE;
  case LESS_EQUAL:          return BinaryOp::LTE;
  case AMPERSAND_AMPERSAND: return BinaryOp::AND;
  case BAR_BAR:             return BinaryOp::OR;
  case AMPERSAND:           return BinaryOp::BIT_AND;
  case BAR:                 return BinaryOp::BIT_OR;
  case CARET:               return BinaryOp::BIT_XOR;
  case LESS_LESS:           return BinaryOp::LEFT_SHIFT;
  case GREATER_GREATER:     return BinaryOp::RIGHT_SHIFT;
  default:                  return BinaryOp::ERROR;
  // clang-format on
  }
}

AssignOp to_assignop(Lexeme l) {
  // clang-format off
  switch (l) {
  case EQUAL:           return AssignOp::ASSIGN;
  case PLUS_EQUAL:      return AssignOp::ADD_ASSIGN;
  case MINUS_EQUAL:     return AssignOp::SUB_ASSIGN;
  case STAR_EQUAL:      return AssignOp::MUL_ASSIGN;
  case SLASH_EQUAL:     return AssignOp::DIV_ASSIGN;
  case MODULO_EQUAL:    return AssignOp::MOD_ASSIGN;
  case AMPERSAND_EQUAL: return AssignOp::BIT_AND_ASSIGN;
  case BAR_EQUAL:       return AssignOp::BIT_OR_ASSIGN;
  case CARET_EQUAL:     return AssignOp::BIT_XOR_ASSIGN;
  case LESS_LESS_EQUAL: return AssignOp::LEFT_SHIFT_ASSIGN;
  case GREATER_GREATER_EQUAL: return AssignOp::RIGHT_SHIFT_ASSIGN;
  default:              return AssignOp::ERROR;
  }
  // clang-format on
}


std::string to_string(BinaryOp b) {
  switch (b) {
  // clang-format off
    case BinaryOp::ADD: return "+";
    case BinaryOp::SUB: return "-";
    case BinaryOp::MUL: return "*";
    case BinaryOp::DIV: return "/";
    case BinaryOp::MOD: return "%";

    case BinaryOp::EQ: return "=";
    case BinaryOp::NEQ: return "!=";
    case BinaryOp::GT: return ">";
    case BinaryOp::LT: return "<";
    case BinaryOp::GTE: return ">=";
    case BinaryOp::LTE: return "<=";

    case BinaryOp::AND: return "&&";
    case BinaryOp::OR: return "||";

    case BinaryOp::BIT_AND: return "&";
    case BinaryOp::BIT_OR: return "|";
    case BinaryOp::BIT_XOR: return "^";
    case BinaryOp::BIT_NOT: return "~";
    case BinaryOp::LEFT_SHIFT: return "<<";
    case BinaryOp::RIGHT_SHIFT: return ">>";
    // clang-format on
    default: return "NOT A BINARY OP";
  }
}

std::string to_string(AssignOp op) {
  switch (op) {
  // clang-format off
    case AssignOp::ASSIGN: return "=";
    case AssignOp::ADD_ASSIGN: return "+=";
    case AssignOp::SUB_ASSIGN: return "-=";
    case AssignOp::MUL_ASSIGN: return "*=";
    case AssignOp::DIV_ASSIGN: return "/=";
    case AssignOp::MOD_ASSIGN: return "%=";
    case AssignOp::BIT_AND_ASSIGN: return "&=";
    case AssignOp::BIT_OR_ASSIGN: return "|=";
    case AssignOp::BIT_XOR_ASSIGN: return "^=";
    case AssignOp::LEFT_SHIFT_ASSIGN: return "<<=";
    case AssignOp::RIGHT_SHIFT_ASSIGN: return ">>=";
    // clang-format on
    default: return "NOT AN ASSIGN OP";
  }
}


}
