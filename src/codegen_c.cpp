#include "codegen_c.hpp"

#include "fmt/core.h"

namespace cinnabar {

CodegenC::CodegenC(const AST &ast): ast(ast) {

}

void CodegenC::add_include(std::string_view file_name) {
  out += fmt::format("#include {}\n", file_name);
}

std::string CodegenC::generate() {
  out = "";
  add_include("<stdint.h>");
  (void) ast;
  return out;
}
  
} // namespace cinnabar
