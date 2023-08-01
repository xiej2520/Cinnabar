#pragma once

#include "ast.hpp"

#include <vector>

namespace cinnabar {

struct TypeResolver {
  AST &ast;

  std::unique_ptr<std::vector<Type>> ast_types = nullptr;

  std::vector<Namespace *> namespaces;
  std::unordered_map<std::string_view, TypeId> builtin_type_map;
  std::unordered_set<TypeId> builtin_numeric_types;
  
  TypeResolver(AST &ast);
  
  void resolve(Stmt &stmt);
  void resolve(Declaration &decl);
  void resolve(FunDecl &fun);
  void resolve(Expr &expr);
  void resolve(Block &block);

  TypeId find_type(std::string_view name);
  TypeId find_type(GenType type);
  TypeId find_binary_op(BinaryOp op, TypeId lhs_type, TypeId rhs_type);

  void resolve();

  [[noreturn]] void error(std::string_view message);
  [[noreturn]] void error(TypeId expected, TypeId found);
};

}
