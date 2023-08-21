#pragma once

#include "ast.hpp"

#include <vector>

namespace cinnabar {

struct TypeResolver {
  AST &ast;

  std::vector<Namespace *> namespaces;
  std::unordered_set<TypeId> builtin_numeric_types;
  
  TypeResolver(AST &ast);

  TypeId get_typeid(const GenericInst &gentype);
  const TypeInst &get_type(TypeId id);

  FunId get_fun(const GenericInst &gentype);

  VarDecl *get_var(std::string_view name);
  DeclPtr get_decl(std::string_view name);

  TypeId find_binary_op(BinaryOp op, TypeId lhs_type, TypeId rhs_type);
  TypeId add_type(const GenericInst &type, TypeDeclPtr decl);
  
  FunId add_fun(const GenericInst &fun, FunDecl *decl);

  void resolve(Stmt &stmt);
  void resolve(Declaration &decl);
  void resolve(FunDecl &fun);
  void resolve(Expr &expr);
  void resolve(Block &block);
  void resolve();

  [[noreturn]] void error(std::string_view message);
  [[noreturn]] void error(TypeId expected, TypeId found);
};

}
