#include "codegen.hpp"

#include "codegen_c.hpp"

namespace cinnabar {


std::string generate(const TAST &tast, CodegenOutput output_type) {
  switch (output_type) {
  case CodegenOutput::C: {
    CodegenC cg(tast);
    return cg.generate();
  break;
  }
  case CodegenOutput::LLVM:
    fmt::print("Currently does not support LLVM backend.");
    abort();
    return "";
  }
  return "";
}


} // namespace cinnbar
