#pragma once

#include "ast.hpp"
#include "tast.hpp"

#include <unordered_set>

namespace cinnabar {

struct TypeResolver {
  class PushNamespace {
    TypeResolver *tr;
    Namespace *prev_namesp;
    TNamespace *prev_tnamesp;

  public:
    inline PushNamespace(
        TypeResolver *tr, Namespace *namesp, TNamespace *tnamesp
    )
        : tr(tr), prev_namesp(tr->cur_ast_namesp),
          prev_tnamesp(tr->cur_tnamesp) {
      tr->cur_ast_namesp = namesp;
      tr->cur_tnamesp = tnamesp;
    }
    PushNamespace(const PushNamespace &) = delete;
    PushNamespace(const PushNamespace &&) = delete;
    PushNamespace operator=(const PushNamespace &) = delete;
    PushNamespace operator=(const PushNamespace &&) = delete;
    inline ~PushNamespace() {
      tr->cur_ast_namesp = prev_namesp;
      tr->cur_tnamesp = prev_tnamesp;
    }
  };
  class PushFun {
    TypeResolver *tr;
    FunctionInst *prev_fun;
    TNamespace *prev_fun_tnamesp;

  public:
    inline PushFun(
        TypeResolver *tr, FunctionInst *new_fun, TNamespace *new_fun_tnamesp
    )
        : tr(tr), prev_fun(tr->cur_fun), prev_fun_tnamesp(tr->cur_fun_tnamesp) {
      tr->cur_fun = new_fun;
      tr->cur_fun_tnamesp = new_fun_tnamesp;
    }
    inline ~PushFun() {
      tr->cur_fun = prev_fun;
      tr->cur_fun_tnamesp = prev_fun_tnamesp;
    }
  };

  AST &ast;

  std::vector<Item> items;
  std::vector<TypeRef> types;

  // avoid cycles, index of items
  std::unordered_set<size_t> currently_creating;
  std::vector<int> type_topo_order;

  Namespace *cur_ast_namesp = nullptr;
  TNamespace *cur_tnamesp = nullptr;

  TNamespace *root_tnamesp = nullptr;

  FunctionInst *cur_fun = nullptr;
  TNamespace *cur_fun_tnamesp = nullptr;

  PushNamespace push_namespace(Namespace *namesp, TNamespace *tnamesp);
  PushFun push_fun(FunctionInst *fun, TNamespace *fun_tnamesp);

  TypeResolver(AST &ast);

  TypeRef get_type(NameWithArgs concrete);
  FunctionInst *get_function(NameWithArgs concrete);

  TVarInst *get_var_local_global(std::string_view name);

  TypeRef find_unary_op(UnaryOp op, const TExpr &operand);
  TypeRef find_binary_op(BinaryOp op, TypeRef lhs_type, TypeRef rhs_type);

  // tnamesp is the TNamespace where the type will be declared
  TypeRef create_struct_type(
      StructDecl *decl, std::span<const GenericArg> args,
      TNamespace *parent_tnamesp
  );
  TypeRef add_type(
      const NameWithArgs &concrete, TypeDeclPtr decl, TNamespace *parent_tnamesp
  );

  // instantiates a concrete typed function from a (potentially generic)
  // untyped function definition
  FunctionInst *
  add_fun(const GenericInst &fun, FunDecl *decl, TNamespace *parent_tnamesp);

  std::optional<TStmt> resolve(Stmt &stmt);
  std::optional<std::unique_ptr<TVarInst>> resolve(Declaration &decl);
  std::unique_ptr<TVarInst> resolve(VarDecl &decl);

  TExpr resolve(Expr &expr);
  std::unique_ptr<TBlock> resolve(Block &block);
  std::unique_ptr<TBlock>
  resolve(Block &block, std::unique_ptr<TNamespace> tnamesp);
  TAST resolve();

  [[noreturn]] void error(std::string_view message);
  [[noreturn]] void error(TypeRef expected, TypeRef found);

  inline bool get_has_error() { return false; }
};

} // namespace cinnabar
