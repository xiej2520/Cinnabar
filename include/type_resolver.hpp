#pragma once

#include "ast.hpp"
#include "tast.hpp"

#include <vector>

namespace cinnabar {

struct TypeResolver {
  class PushNamespace {
    TypeResolver &tr;
    Namespace *prev_namesp;
    TNamespace *prev_tnamesp;
  public:
    PushNamespace(TypeResolver &tr, Namespace *namesp, TNamespace *tnamesp):
        tr(tr), prev_namesp(tr.cur_ast_namesp), prev_tnamesp(tr.cur_tnamesp) {
      tr.cur_ast_namesp = namesp;
      tr.cur_tnamesp = tnamesp;
    }
    ~PushNamespace() {
      tr.cur_ast_namesp = prev_namesp;
      tr.cur_tnamesp = prev_tnamesp;
    }
  };

  AST &ast;

  std::unordered_set<TypeId> builtin_numeric_types;
  
  std::vector<TFunInst> functions;
  std::vector<TTypeInst> types;
  std::vector<std::unique_ptr<BuiltinType>> builtin_types;
  std::unordered_map<std::string_view, TypeId> builtin_type_map;
  
  Namespace *cur_ast_namesp;
  TNamespace *cur_tnamesp;

  // avoid cycles
  std::unordered_set<std::string> currently_creating;
  
  PushNamespace push_namespace(Namespace *namesp, TNamespace *tnamesp);

  TypeResolver(AST &ast);

  TypeId get_typeid(const GenericInst &gentype);
  const TTypeInst &get_type(TypeId id);

  FunId get_funid(const GenericInst &gentype);
  const TFunInst &get_fun(FunId id);

  TVarInst *get_var(std::string_view name);
  DeclaredName get_decl(const GenericInst &geninst);

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
