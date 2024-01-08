#pragma once

#include "tast.hpp"


namespace cinnabar {

enum class CodegenOutput {
  C,
  LLVM,
};

std::string generate(const TAST &tast, CodegenOutput output_type);

} // namespace cinnbar
