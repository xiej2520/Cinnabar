#include "tast.hpp"

namespace cinnabar {

TypeId::TypeId() : value(-1) {}
TypeId::TypeId(int i) : value(i) {}

FunId::FunId() : value(-1) {}
FunId::FunId(int i) : value(i) {}

TNamespace::TNamespace(TNamespace *parent) : parent(parent) {}

TExpr::TExpr() : node(std::monostate{}) {}
TExpr::TExpr(TExprVariant node) : node(std::move(node)) {}

bool TIf::has_else() {
  auto &last_condition = branches.back()->condition;
  if (std::unique_ptr<TLiteral> *lit = std::get_if<std::unique_ptr<TLiteral>>(&last_condition.node)) {
    if (bool *b = std::get_if<bool>(&((*lit)->val))) {
      return *b;
    }
    return false;
  }
  return false;
}

TypeId TExpr::type() {
  // clang-format off
  return std::visit(overload{
    [&](std::monostate) { return TypeId{-1}; },
    [&](std::unique_ptr<TBinary> &expr) { return expr->type; },
    [&](std::unique_ptr<TBlock> &expr) { return expr->type; },
    [&](std::unique_ptr<TDotRef> &expr) { return expr->type; },
    [&](std::unique_ptr<TFunCall> &expr) { return expr->type; },
    [&](std::unique_ptr<TIf> &expr) { return expr->type; },
    [&](std::unique_ptr<TLiteral> &expr) { return expr->type; },
    [&](std::unique_ptr<TUnary> &expr) { return expr->type; },
    [&](std::unique_ptr<TVariable> &expr) { return expr->type; },
  }, node);
  // clang-format on
}

std::string TTypeInst::name() const {
  // clang-format off
  return std::visit(overload{
    [&](const BuiltinType &bt) { return std::string(bt.name.str); },
    [&](const TEnumInst &inst) { return inst.concrete_type.to_string(); },
    [&](const TStructInst &inst) { return inst.concrete_type.to_string(); }
  }, def);
  // clang-format on
}

std::string TFunInst::name() const {
  return concrete_fun.to_string();
}

} // namespace cinnabar
