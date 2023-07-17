#pragma once

#include "ast.hpp"

#include <vector>

namespace cinnabar {

struct TypeResolver {
  const AST &ast;

  std::unique_ptr<std::vector<Type>> ast_types = nullptr;

  std::vector<Namespace *> namespaces;
  
  TypeResolver(const AST &ast);
  
  TypeId find_type(std::string_view name);
  TypeId find_binary_op(BinaryOp op, TypeId lhs_type, TypeId rhs_type);

  void resolve();
};

}
