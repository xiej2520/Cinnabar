#pragma once

#include "ast_ops.hpp"
#include "common.hpp"
#include "lexer.hpp"

#include "ast.hpp"

#include <memory>
#include <optional>
#include <unordered_set>
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
// declaration statements
struct TEnumInst;
struct TFunInst;
struct TStructInst;
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
struct TVariable;
struct TUnary;

struct TypeId {
  int value;

  TypeId();
  TypeId(int i);
  operator int() const { return value; }
};

struct FunId {
  int value;

  FunId();
  FunId(int i);
  operator int() const { return value; }
};

struct TNamespace {
  // concrete types/funcs don't always have tokens, own the strings
  std::unordered_map<std::string, TypeId> concrete_types;
  std::unordered_map<std::string, FunId> concrete_funs;
  std::unordered_map<std::string_view, TVarInst *> variables;

  TNamespace *parent;
  TNamespace(TNamespace *parent);
};

// monostate because we need default value for node for TExpr construction
using TExprVariant = std::variant<
    std::monostate,
    std::unique_ptr<TBinary>, std::unique_ptr<TBlock>, std::unique_ptr<TDotRef>,
    std::unique_ptr<TFunCall>, std::unique_ptr<TIf>, std::unique_ptr<TIndex>,
    std::unique_ptr<TLiteral>, std::unique_ptr<TUnary>, std::unique_ptr<TVariable>>;

struct TExpr {
  TExprVariant node;

  [[nodiscard]] TypeId type() const;
  [[nodiscard]] bool is_place_expr() const;

  TExpr();
  TExpr(TExprVariant node);
  template <typename T> bool is() { return std::holds_alternative<T>(node); }
  template <typename T> T &as() { return std::get<T>(node); }
};

struct TBinary {
  TypeId type = -1;
  BinaryOp op;
  TExpr left;
  TExpr right;
};

struct TBlock {
  TypeId type = -1;
  std::vector<TStmt> stmts;
  std::unique_ptr<TNamespace> namesp;
};

struct TDotRef {
  TypeId type = -1;
  FunId fun = -1; // for method
  int prop_idx = -1; // for field or variant
  TExpr left;
  Token name;
};

struct TFunCall {
  TypeId type = -1;
  FunId fun = -1;
  TExpr callee;
  std::vector<TExpr> args;
};

struct TIf {
  TypeId type = -1;
  struct TBranch {
    // else branch will be a Literal true condition as the last block
    TExpr condition;
    std::unique_ptr<TBlock> block;
  };
  std::vector<std::unique_ptr<TBranch>> branches;
  
  bool has_else();
};

struct TIndex {
  TypeId type = -1;
  TExpr callee;
  TExpr arg; // only handle one arg for now
};

using LiteralVariant =
    std::variant<int32_t, int64_t, float, double, bool, char, std::string>;

struct TLiteral {
  TypeId type = -1;
  LiteralVariant val;
};

using DeclaredName = std::variant<FunId, TypeId, TVarInst *>;

struct TVariable {
  TypeId type;
  Token name;
  DeclaredName decl;
};

struct TUnary {
  TypeId type;
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
  TypeId type;
  std::optional<TExpr> initializer;
};

struct TWhile {};

struct TPrint {
  std::vector<TExpr> args;
  bool newline;
};

using TStmtVariant = std::variant<
    std::unique_ptr<TAssign>, std::unique_ptr<TBreak>,
    std::unique_ptr<TContinue>, TExpr,
    std::unique_ptr<TFor>, std::unique_ptr<TReturn>, std::unique_ptr<TVarInst>,
    std::unique_ptr<TWhile>, std::unique_ptr<TPrint>>;

struct TStmt {
  TStmtVariant node;
  template <typename T> bool is() { return std::holds_alternative<T>(node); }
  template <typename T> T &as() { return std::get<T>(node); }
};

struct TFunInst {
  GenericInst concrete_fun;
  std::vector<TypeId> generic_args;
  std::vector<std::unique_ptr<TVarInst>> params;
  TypeId return_type;
  std::unique_ptr<TBlock> body;
  
  [[nodiscard]] std::string name() const;
};

enum TBuiltinEnum : std::size_t {
  Ref = 0,
  VarRef = 1,
  Span = 2,
  VarSpan = 3,
  Array = 4,
};

struct TBuiltinType {
  GenericInst concrete_type;
  std::variant<TypeId, TypeId, TypeId, TypeId, std::pair<TypeId, size_t>> args;
};

struct TEnumInst {
  GenericInst concrete_type;
  std::vector<TypeId> generic_args;
  std::unique_ptr<TNamespace> namesp;
  // name: {type, index}
  std::unordered_map<std::string_view, std::pair<TypeId, int>> variants;
  std::unordered_map<std::string_view, FunId> methods;
};

struct TStructInst {
  GenericInst concrete_type;
  std::vector<TypeId> generic_args;
  std::unique_ptr<TNamespace> namesp;
  // name: {type, index}
  std::unordered_map<std::string_view, std::pair<TypeId, int>> fields;
  std::unordered_map<std::string_view, FunId> methods;
};

struct TTypeInst {
  std::variant<Primitive, TBuiltinType, TEnumInst, TStructInst> def;
  
  [[nodiscard]] std::string name() const;
  [[nodiscard]] GenericInst concrete_type() const;
  template <typename T> bool is() { return std::holds_alternative<T>(def); }
  template <typename T> T &as() { return std::get<T>(def); }
};


struct TAST {
  std::vector<TFunInst> functions;
  std::vector<TTypeInst> types;
  std::vector<TypeId> type_topo_order;
  // TypeId for primitive types, as well as Span[char]
  std::unordered_map<std::string_view, TypeId> primitive_map;
  
  std::vector<std::unique_ptr<TVarInst>> globals;
  
  std::unique_ptr<TNamespace> root_namesp;
};

} // namespace cinnabar


template<>
struct std::hash<cinnabar::TypeId> {
  std::size_t operator()(const cinnabar::TypeId &id) const { return id; }
};
