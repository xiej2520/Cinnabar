#include "codegen.hpp"

#include "codegen_c.hpp"

#include "fmt/core.h"

namespace cinnabar {

  
std::string generate(const AST &ast, CodegenOutput output_type) {
  switch (output_type) {
  case CodegenOutput::C: {
    CodegenC cg(ast);
    return cg.generate();
  break;
  }
  case CodegenOutput::LLVM:
    fmt::print("Currently does not support LLVM backend.");
    abort();
    return "";
  }
  
}

  
} // namespace cinnbar
