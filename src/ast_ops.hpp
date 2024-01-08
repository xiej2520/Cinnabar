#pragma once

#include "lexer.hpp"

namespace cinnabar {

enum class UnaryOp {
  PLUS,
  NEG,
  NOT,
  REF,
  VARREF,
  DEREF,

  ERROR,
};

UnaryOp to_unaryop(Lexeme l);
std::string to_string(UnaryOp u);

enum class BinaryOp {
  ADD,
  SUB,
  MUL,
  DIV,
  MOD,

  EQ,
  NEQ,
  GT,
  LT,
  GTE,
  LTE,

  AND,
  OR,

  XOR,

  BIT_AND,
  BIT_OR,
  LEFT_SHIFT,
  RIGHT_SHIFT,

  ERROR,
};

BinaryOp to_binop(Lexeme l);
std::string to_string(BinaryOp b);

enum class AssignOp {
  ASSIGN,
  ADD_ASSIGN,
  SUB_ASSIGN,
  MUL_ASSIGN,
  DIV_ASSIGN,
  MOD_ASSIGN,
  BIT_AND_ASSIGN,
  BIT_OR_ASSIGN,
  BIT_XOR_ASSIGN,
  LEFT_SHIFT_ASSIGN,
  RIGHT_SHIFT_ASSIGN,

  ERROR,
};

AssignOp to_assignop(Lexeme l);
std::string to_string(AssignOp op);

}
