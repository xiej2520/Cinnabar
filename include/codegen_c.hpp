#pragma once

#include "codegen.hpp"

namespace cinnabar {

class CodegenC {

  const AST &ast;

  std::string out;
  
  void add_include(std::string_view file_name);

public:
  
  CodegenC(const AST &ast);
  
  std::string generate();

};


} // namespace cinnabar
