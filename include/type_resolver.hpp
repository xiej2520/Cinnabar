#pragma once

#include "ast.hpp"
#include "tast.hpp"

#include <vector>

namespace cinnabar {

struct TypeResolver {
  class PushNamespace {
    TypeResolver *tr;
    Namespace *prev_namesp;
    TNamespace *prev_tnamesp;
  public:
    inline PushNamespace(TypeResolver *tr, Namespace *namesp, TNamespace *tnamesp):
        tr(tr), prev_namesp(tr->cur_ast_namesp), prev_tnamesp(tr->cur_tnamesp) {
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
    TFunInst *prev_fun;
    TNamespace *prev_fun_tnamesp;
  public:
    inline PushFun(TypeResolver *tr, TFunInst *new_fun, TNamespace *new_fun_tnamesp):
        tr(tr), prev_fun(tr->cur_fun), prev_fun_tnamesp(tr->cur_fun_tnamesp) {
      tr->cur_fun = new_fun;
      tr->cur_fun_tnamesp = new_fun_tnamesp;
    }
    inline ~PushFun() {
      tr->cur_fun = prev_fun;
      tr->cur_fun_tnamesp = prev_fun_tnamesp;
    }
  };

  AST &ast;

  std::vector<TFunInst> functions;
  std::vector<TTypeInst> types;
  std::vector<TypeId> type_topo_order;
  std::vector<std::unique_ptr<BuiltinType>> builtin_types;
  std::unordered_map<std::string_view, TypeId> primitive_map;
  std::unordered_set<TypeId> numeric_primitive_ids;
  
  Namespace *cur_ast_namesp = nullptr;
  TNamespace *cur_tnamesp = nullptr;
  
  TNamespace *root_tnamesp = nullptr;
  
  TFunInst *cur_fun = nullptr;
  TNamespace *cur_fun_tnamesp = nullptr;

  // avoid cycles
  std::unordered_set<std::string> currently_creating;
  
  PushNamespace push_namespace(Namespace *namesp, TNamespace *tnamesp);
  PushFun push_fun(TFunInst *fun, TNamespace *fun_tnamesp);

  TypeResolver(AST &ast);

  TypeId get_typeid(const GenericInst &gentype);
  const TTypeInst &get_type(TypeId id);

  FunId get_funid(const GenericInst &gentype);
  const TFunInst &get_fun(FunId id);

  TVarInst *get_var_local_global(std::string_view name);

  DeclaredName get_decl(const GenericInst &geninst);

  TypeId find_unary_op(UnaryOp op, TypeId operand_type);
  TypeId find_binary_op(BinaryOp op, TypeId lhs_type, TypeId rhs_type);

  // tnamesp is the TNamespace where the type will be declared
  TypeId add_type(const GenericInst &type, TypeDeclPtr decl, TNamespace *parent_tnamesp);
  
  FunId add_fun(const GenericInst &fun, FunDecl *decl, TNamespace *parent_tnamesp);

  std::optional<TStmt> resolve(Stmt &stmt);
  std::optional<std::unique_ptr<TVarInst>> resolve(Declaration &decl);
  std::unique_ptr<TVarInst> resolve(VarDecl &decl);

  TExpr resolve(Expr &expr);
  std::unique_ptr<TBlock> resolve(Block &block);
  std::unique_ptr<TBlock> resolve(Block &block, std::unique_ptr<TNamespace> tnamesp);
  TAST resolve();

  [[noreturn]] void error(std::string_view message);
  [[noreturn]] void error(TypeId expected, TypeId found);
};

}
