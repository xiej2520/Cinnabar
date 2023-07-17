#pragma once

#include "ast.hpp"
#include "tast.hpp"

#include <vector>

namespace cinnabar {

struct TypeResolver {
  const AST &ast;

  std::unique_ptr<std::vector<Type>> tast_types = nullptr;

  std::vector<TNamespace *> namespaces;
  
  TypeResolver(const AST &ast);
  
  TypeId reserve_type();
  void link_type(TypeId id, TTypeDeclPtr type_decl_ptr);
  
  TypeId find_type(std::string_view name);
  TypeId find_binary_op(BinaryOp op, TypeId lhs_type, TypeId rhs_type);

  TStmt resolve(const Stmt &stmt);
  std::unique_ptr<TBlock> resolve(const Block &block);
  std::unique_ptr<TFunDecl> resolve(const FunDecl &fun);
  TDeclaration resolve(const Declaration &decl);
  TExpr resolve(const Expr &expr);
  TAST resolve();
};

}
