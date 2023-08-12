#pragma once

#include "ast.hpp"

#include <vector>

namespace cinnabar {

struct TypeResolver {
  AST &ast;

  std::vector<Namespace *> namespaces;
  std::unordered_map<std::string_view, TypeId> builtin_type_map;
  std::unordered_set<TypeId> builtin_numeric_types;
  
  TypeResolver(AST &ast);

  TypeId get_typeid(const GenType &gentype);
  const Type &get_type(TypeId id);

  // for now, generic function later
  FunDecl *get_fun(std::string_view name);
  VarDecl *get_var(std::string_view name);
  DeclPtr get_decl(std::string_view name);

  
  TypeId find_binary_op(BinaryOp op, TypeId lhs_type, TypeId rhs_type);
  TypeId add_type(const GenType &type, TypeDeclPtr decl);

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
