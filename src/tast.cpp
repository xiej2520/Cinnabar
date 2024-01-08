#include "tast.hpp"

namespace cinnabar {

std::string Path::to_string(std::span<const TypeRef> types) const {
  return std::visit(overload{
    [](EnumGen *) { return std::string{}; },
    [](StructGen *) { return std::string{}; },
    [](FunctionGen *) { return std::string{}; },
    [&](EnumInst *i) { return i->to_string(types); },
    [&](StructInst *i) { return i->to_string(types); },
    [&](FunctionInst *i) { return i->to_string(types); },
  }, path_value);
}

const char *name(Primitive p) {
  switch (p) {
  case PRIM_BOOL: return "bool";
  case PRIM_I8: return "i8";
  case PRIM_I16: return "i16";
  case PRIM_I32: return "i32";
  case PRIM_I64: return "i64";
  case PRIM_I128: return "i128";
  case PRIM_ISIZE: return "isize";
  case PRIM_U8: return "u8";
  case PRIM_U16: return "u16";
  case PRIM_U32: return "u32";
  case PRIM_U64: return "u64";
  case PRIM_U128: return "u128";
  case PRIM_USIZE: return "usize";
  case PRIM_F32: return "f32";
  case PRIM_F64: return "f64";
  }
  return "UNKNOWN PRIMITIVE TYPE";
}

// clang-format off
//TypeRef TypeRef::clone() const {
//  return std::visit(overload{
//    [](None) { return TypeRef{None{}}; },
//    [](Unit) { return TypeRef{Unit{}}; },
//    [](Primitive p) { return TypeRef{p}; },
//    [](const FunctionType &f) {
//      std::vector<TypeRef> arg_types{};
//      arg_types.reserve(f.arg_types.size());
//      for (const auto &tr : f.arg_types) {
//        arg_types.push_back(tr.clone());
//      }
//      return TypeRef{FunctionType{std::move(arg_types), std::make_unique<TypeRef>(f.return_type->clone())}};
//    },
//    [](const Ref &r) { return TypeRef{Ref{r.is_mut, std::make_unique<TypeRef>(r.arg->clone())}}; },
//    [](const Array &a) { return TypeRef{Array{std::make_unique<TypeRef>(a.arg->clone()), a.size}}; },
//    [](const Span &s) { return TypeRef{Span{s.is_mut, std::make_unique<TypeRef>(s.arg->clone())}}; },
//    [](Path p) { return TypeRef{p}; },
//  }, data);
//}

std::string TypeRef::to_string(std::span<const TypeRef> types) const {
  return std::visit(overload{
    [](None) { throw "Called to_string() on None type"; return std::string{}; },
    [](Unit) { return std::string{"()"}; },
    [](Primitive p) { return std::string{name(p)}; },
    [&](const FunctionType &f) {
      std::string res{'('};
      if (!f.arg_types.empty()) {
        for (TypeId id : f.arg_types) {
          res += types[id].to_string(types);
          res += ',';
        }
        res.back() = ')';
      } else { res += ')'; }
      res += types[f.return_type].to_string(types);
      return res;
    },
    [&](const Ref &r) { return fmt::format("Ref[{}]", types[r.arg].to_string(types)); },
    [&](const Array &a) {
      std::string res{"Array["};
      res += types[a.arg].to_string(types);
      if (a.size.has_value()) {
        res += ',';
        res += std::to_string(a.size.value());
      }
      res += ']';
      return res;
    },
    [&](const Span &s) { return fmt::format("Span[{}]", types[s.arg].to_string(types)); },
    [&](Path p) { return p.to_string(types); },
  }, data);
}

//bool TypeRef::operator==(const TypeRef &other) const {
//  if (data.index() != other.data.index()) {
//    return false;
//  }
//  return std::visit(overload{
//    [](None) { return true; },
//    [](Unit) { return true; },
//    [&](Primitive p) { return std::get<Primitive>(other.data) == p; },
//    [&](const FunctionType &f) {
//      return std::get<FunctionType>(other.data).arg_types == f.arg_types &&
//          *std::get<FunctionType>(other.data).return_type == *f.return_type;
//    },
//    [&](const Ref &r) {
//      return std::get<Ref>(other.data).is_mut == r.is_mut &&
//          *std::get<Ref>(other.data).arg == *r.arg;
//    },
//    [&](const Array &a) {
//      return std::get<Array>(other.data).size == a.size &&
//          *std::get<Array>(other.data).arg == *a.arg;
//    },
//    [&](const Span &s) {
//      return std::get<Span>(other.data).is_mut == s.is_mut &&
//          *std::get<Span>(other.data).arg == *s.arg;
//    },
//    [&](const Path &p) {
//      return std::get<Path>(other.data) == p;
//    },
//  }, data);
//}

// clang-format on

std::string to_string(Token base, std::span<const GenericArg> args, std::span<const TypeRef> types) {
  std::string res{base.str};
  if (!args.empty()) {
    res += '[';
    for (const auto &child : args) {
      res += to_string(child, types);
      res += ',';
    }
    res.back() = ']'; // replace last comma
  }
  return res;
}

std::string to_string(const GenericArg &arg, std::span<const TypeRef> types) {
  return std::visit(overload{
    [&](const TypeRef &t) { return t.to_string(types); },
    [](Literal l) { return to_string(l.val); },
  }, arg);
}

std::string FunctionInst::to_string(std::span<const TypeRef> types) const {
  return ::cinnabar::to_string(base_name, generic_args, types);
}

std::string EnumInst::to_string(std::span<const TypeRef> types) const {
  return ::cinnabar::to_string(base_name, generic_args, types);
}

std::string StructInst::to_string(std::span<const TypeRef> types) const {
  return ::cinnabar::to_string(base_name, generic_args, types);
}

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

//TypeRef TExpr::type() const {
//  // clang-format off
//  return std::visit(overload{
//    [&](std::monostate) { return TypeRef{None{}}; },
//    [&](const std::unique_ptr<TBinary> &expr) { return expr->type.clone(); },
//    [&](const std::unique_ptr<TBlock> &expr) { return expr->type.clone(); },
//    [&](const std::unique_ptr<TDotRef> &expr) { return expr->type.clone(); },
//    [&](const std::unique_ptr<TFunCall> &expr) { return expr->type.clone(); },
//    [&](const std::unique_ptr<TIf> &expr) { return expr->type.clone(); },
//    [&](const std::unique_ptr<TIndex> &expr) { return expr->type.clone(); },
//    [&](const std::unique_ptr<TLiteral> &expr) { return expr->type.clone(); },
//    [&](const std::unique_ptr<TUnary> &expr) { return expr->type.clone(); },
//    [&](const std::unique_ptr<TFunctionName> &expr) { return expr->type.clone(); },
//    [&](const std::unique_ptr<TTypeName> &expr) { return expr->type.clone(); },
//    [&](const std::unique_ptr<TVariable> &expr) { return expr->type.clone(); },
//  }, node);
//  // clang-format on
//}

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
    [&](const std::unique_ptr<TFunctionName> &) { return false; },
    [&](const std::unique_ptr<TTypeName> &) { return false; },
    [&](const std::unique_ptr<TVariable> &) { return true; },
  }, node);
  // clang-format on
}

} // namespace cinnabar
