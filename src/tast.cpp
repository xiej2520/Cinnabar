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

TypeId TExpr::type() const {
  // clang-format off
  return std::visit(overload{
    [&](std::monostate) { return TypeId{-1}; },
    [&](const std::unique_ptr<TBinary> &expr) { return expr->type; },
    [&](const std::unique_ptr<TBlock> &expr) { return expr->type; },
    [&](const std::unique_ptr<TDotRef> &expr) { return expr->type; },
    [&](const std::unique_ptr<TFunCall> &expr) { return expr->type; },
    [&](const std::unique_ptr<TIf> &expr) { return expr->type; },
    [&](const std::unique_ptr<TIndex> &expr) { return expr->type; },
    [&](const std::unique_ptr<TLiteral> &expr) { return expr->type; },
    [&](const std::unique_ptr<TUnary> &expr) { return expr->type; },
    [&](const std::unique_ptr<TVariable> &expr) { return expr->type; },
  }, node);
  // clang-format on
}

bool TExpr::is_place_expr() const {
  // clang-format off
  return std::visit(overload{
    [&](std::monostate) { return false; },
    [&](const std::unique_ptr<TBinary> &) { return false; },
    [&](const std::unique_ptr<TBlock> &) { return false; },
    [&](const std::unique_ptr<TDotRef> &) { return true; },
    [&](const std::unique_ptr<TFunCall> &) { return false; },
    [&](const std::unique_ptr<TIf> &) { return false; },
    [&](const std::unique_ptr<TIndex> &) { return true; },
    [&](const std::unique_ptr<TLiteral> &) { return false; },
    [&](const std::unique_ptr<TUnary> &expr) { return expr->op == UnaryOp::DEREF; },
    [&](const std::unique_ptr<TVariable> &expr) { return std::holds_alternative<TVarInst *>(expr->decl); },
  }, node);
  // clang-format on
}

// clang-format off
[[nodiscard]] std::string TTypeInst::name() const {
  return std::visit(overload{
    [&](const Primitive &p) { return std::string(p.name.str); },
    [&](const TBuiltinType &bt) { return bt.concrete_type.to_string(); },
    [&](const TEnumInst &inst) { return inst.concrete_type.to_string(); },
    [&](const TStructInst &inst) { return inst.concrete_type.to_string(); }
  }, def);
}

[[nodiscard]] GenericInst TTypeInst::concrete_type() const {
  return std::visit(overload{
    [&](const Primitive &p) { return GenericInst{p.name.str}; },
    [&](const TBuiltinType &bt) { return bt.concrete_type; },
    [&](const TEnumInst &inst) { return inst.concrete_type; },
    [&](const TStructInst &inst) { return inst.concrete_type; }
  }, def);
}
// clang-format on


[[nodiscard]] std::string TFunInst::name() const {
  return concrete_fun.to_string();
}

} // namespace cinnabar
