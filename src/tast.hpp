#pragma once

#include "ast_ops.hpp"
#include "common.hpp"
#include "lexer.hpp"

#include "ast.hpp"

#include <memory>
#include <optional>
#include <span>
#include <variant>
#include <vector>

namespace cinnabar {

struct TStmt;
struct TAssign;
struct TBreak;
struct TContinue;
struct TDeclaration;
struct TFor;
struct TReturn;
struct TWhile;

// don't want to implement variadics or macros yet
struct Print;

struct EnumGen;
struct StructGen;
struct FunctionGen;

struct EnumInst;
struct StructInst;
struct FunctionInst;

struct TVarInst;

// expressions
struct TExpr;
struct TBinary;
struct TBlock;
struct TDotRef;
struct TFunCall;
struct TIf;
struct TIndex;
struct TLiteral;
struct TFunctionName;
struct TTypeName;
struct TVariable;
struct TUnary;

// types

struct TypeRef;

struct FunctionType;
struct Ref;
struct Array;
struct Span;
struct Path;

// path item: enum, struct, function
struct Path {
  std::variant<
      EnumGen *, StructGen *, FunctionGen *, EnumInst *, StructInst *,
      FunctionInst *>
      path_value;

  size_t namespace_id;
  size_t item_index; // index of path_value in tast.items

  template <typename T>
  T *get_if() { return std::get_if<T>(&path_value); }

  bool operator==(const Path &other) const = default;
  std::string to_string(std::span<const TypeRef> types) const;
};

using TypeId = int;

struct None {};
struct Unit {};

// clang-format off
enum Primitive {
  PRIM_BOOL,
  PRIM_I8, PRIM_I16, PRIM_I32, PRIM_I64, PRIM_I128, PRIM_ISIZE,
  PRIM_U8, PRIM_U16, PRIM_U32, PRIM_U64, PRIM_U128, PRIM_USIZE,
  PRIM_F32, PRIM_F64
};
extern std::vector<Primitive> primitive_types;
// clang-format on

const char *name(Primitive p);

struct FunctionType {
  std::vector<TypeId> arg_types;
  TypeId return_type;
};

struct Ref {
  bool is_mut;
  TypeId arg;
};

struct Array {
  TypeId arg;
  std::optional<size_t> size;
};

struct Span {
  bool is_mut;
  TypeId arg;
};

using TypeData =
    std::variant<None, Unit, Primitive, FunctionType, Ref, Array, Span, Path>;

struct TypeRef {
  TypeData data;

  // clang-format off
  template <typename T>
  bool is() { return std::holds_alternative<T>(data); }

  template <typename T>
  T &as() { return std::get<T>(data); }

  template <typename T>
  T *get_if() { return std::get_if<T>(&data); }
  // clang-format on
  bool operator==(const TypeRef &other) const;
  std::string to_string(const std::span<const TypeRef> types) const;
};

struct TypeParam {
  Token name;
};

struct ValueParam {
  Token name;
  TypeRef type_of;
};

using TGenericParam = std::variant<TypeParam, ValueParam>;

struct TGenericArg {
  std::variant<TypeRef, std::unique_ptr<TVarInst>> data;
  [[nodiscard]] std::string to_string(std::span<const TypeRef> types) const;
};

std::string to_string(
    Token base, std::span<const TGenericArg> args,
    std::span<const TypeRef> types
);

struct TGenericInst {
  Token base_name;
  std::vector<TGenericArg> args;
  [[nodiscard]] std::string to_string(std::span<const TypeRef> types) const;
};
} // namespace cinnabar

//// need hashes and equality operator for TNamespace
//// keeping strings as the key isn't enough because we can nest definitions
// template <>
// struct std::hash<cinnabar::TypeRef> {
//   std::size_t operator()(const cinnabar::TypeRef &type) const {
//     // lazy
//     return std::hash<std::string>{}(type.to_string());
//   }
// };
//
// template <>
// struct std::hash<cinnabar::Literal> {
//   std::size_t operator()(const cinnabar::Literal &lit) const {
//     return std::hash<cinnabar::LiteralVariant>{}(lit.val);
//   }
// };
//
// template <>
// struct std::hash<cinnabar::TGenericInst> {
//   std::size_t operator()(const cinnabar::TGenericInst &name) const {
//     std::size_t res = std::hash<std::string_view>{}(name.base_name.str);
//     res = std::rotl(res, 1) ^ std::hash<std::size_t>{}(name.args.size());
//     for (const cinnabar::TGenericArg &arg : name.args) {
//       res = std::rotl(res, 1) ^ std::hash<cinnabar::TGenericArg>{}(arg);
//     }
//     return res;
//   }
// };

namespace cinnabar {

using ItemRef = std::variant<TypeRef, FunctionInst *, TVarInst *>;

struct TNamespace {
  // concrete types/funcs don't always have tokens, own the strings
  std::unordered_map<std::string, ItemRef> items;
  size_t id;
  TNamespace *parent;
  TNamespace(size_t id, TNamespace *parent);
};

struct FunctionGen {};

struct FunctionInst {
  Token base_name;
  std::vector<TGenericArg> generic_args;
  std::vector<std::unique_ptr<TVarInst>> params;
  TypeRef return_type = {None{}};

  std::unique_ptr<TBlock> body;

  std::string to_string(std::span<const TypeRef> types) const;
};

struct EnumGen {};

struct EnumInst {
  Token base_name;
  std::vector<TGenericArg> generic_args;
  std::unique_ptr<TNamespace> namesp;

  // name: {type, index}
  std::unordered_map<std::string_view, std::pair<TypeRef, int>> variants;
  std::unordered_map<std::string_view, FunctionInst *> methods;

  std::string to_string(std::span<const TypeRef> types) const;
};

struct StructGen {};

struct StructInst {
  Token base_name;
  std::vector<TGenericArg> generic_args;
  std::unique_ptr<TNamespace> namesp;

  // name: {type, index}
  std::unordered_map<std::string_view, std::pair<TypeRef, int>> fields;
  std::unordered_map<std::string_view, FunctionInst *> methods;

  std::string to_string(std::span<const TypeRef> types) const;
};

// monostate because we need default value for node for TExpr construction
using TExprVariant = std::variant<
    std::monostate, std::unique_ptr<TBinary>, std::unique_ptr<TBlock>,
    std::unique_ptr<TDotRef>, std::unique_ptr<TFunCall>, std::unique_ptr<TIf>,
    std::unique_ptr<TIndex>, std::unique_ptr<TLiteral>, std::unique_ptr<TUnary>,
    std::unique_ptr<TFunctionName>, std::unique_ptr<TTypeName>,
    std::unique_ptr<TVariable>>;

struct TExpr {
  TExprVariant node;

  [[nodiscard]] TypeRef type() const;
  [[nodiscard]] bool is_place_expr() const;

  TExpr();
  TExpr(TExprVariant node);
  template <typename T>
  bool is() { return std::holds_alternative<T>(node); }

  template <typename T>
  T &as() { return std::get<T>(node); }

  template <typename T>
  T *get_node_if() { return std::get_if<std::unique_ptr<T>>(&node)->get(); }
};

struct TBinary {
  TypeRef type = {None{}};
  BinaryOp op;
  TExpr left;
  TExpr right;
};

struct TBlock {
  TypeRef type = {None{}};
  std::vector<TStmt> stmts;
  std::unique_ptr<TNamespace> namesp;
};

struct TDotRef {
  TypeRef type = {None{}};
  FunctionInst *fun = nullptr; // for method
  int prop_idx = -1;           // for field or variant
  TExpr left;
  Token name;
};

struct TFunCall {
  TypeRef type = {None{}};
  FunctionInst *fun = nullptr; // for method
  TExpr callee;
  std::vector<TExpr> args;
};

struct TIf {
  TypeRef type = {None{}};
  struct TBranch {
    // else branch will be a Literal true condition as the last block
    TExpr condition;
    std::unique_ptr<TBlock> block;
  };
  std::vector<std::unique_ptr<TBranch>> branches;

  bool has_else();
};

struct TIndex {
  TypeRef type = {None{}};
  TExpr callee;
  TExpr arg; // only handle one arg for now
};

using LiteralVariant =
    std::variant<int32_t, int64_t, float, double, bool, char, std::string>;

struct TLiteral {
  TypeRef type = {None{}};
  LiteralVariant val;
};

struct TFunctionName {
  TypeRef type = {None{}};
  FunctionInst *fun;
};

struct TTypeName {
  TypeRef type = {None{}};
};

struct TVariable {
  TypeRef type = {None{}};
  Token name;
  TVarInst *inst;
};

struct TUnary {
  TypeRef type = {None{}};
  UnaryOp op;
  TExpr operand;
};

struct TAssign {
  AssignOp op;
  TExpr lhs;
  TExpr rhs;
};

struct TBreak {};
struct TContinue {};

struct TFor {};

struct TReturn {
  std::optional<TExpr> value;
};

struct TVarInst {
  Token name;
  TypeRef type = {None{}};
  std::optional<TExpr> initializer;
};

struct TWhile {};

struct TPrint {
  std::vector<TExpr> args;
  bool newline;
};

using TStmtVariant = std::variant<
    std::unique_ptr<TAssign>, std::unique_ptr<TBreak>,
    std::unique_ptr<TContinue>, TExpr, std::unique_ptr<TFor>,
    std::unique_ptr<TReturn>, std::unique_ptr<TVarInst>,
    std::unique_ptr<TWhile>, std::unique_ptr<TPrint>>;

struct TStmt {
  TStmtVariant node;
  template <typename T>
  bool is() {
    return std::holds_alternative<T>(node);
  }
  template <typename T>
  T &as() {
    return std::get<T>(node);
  }
};

using Item = std::variant<
    std::unique_ptr<StructGen>, std::unique_ptr<StructInst>,
    std::unique_ptr<EnumGen>, std::unique_ptr<EnumInst>,
    std::unique_ptr<FunctionGen>, std::unique_ptr<FunctionInst>>;

struct TAST {
  std::vector<int> type_topo_order;

  std::vector<std::unique_ptr<TVarInst>> globals;
  std::vector<Item> items;

  std::vector<TypeRef> types;

  std::unique_ptr<TNamespace> root_namesp;
  std::vector<TNamespace *> namespaces;
};

} // namespace cinnabar

// template <>
// struct std::hash<cinnabar::TypeId> {
//   std::size_t operator()(const cinnabar::TypeId &id) const { return id; }
// };
