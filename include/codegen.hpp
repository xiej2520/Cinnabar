#pragma once

#include "ast.hpp"


namespace cinnabar {

enum class CodegenOutput {
  C,
  LLVM,
};

std::string generate(const AST &ast, CodegenOutput output_type);

} // namespace cinnbar
