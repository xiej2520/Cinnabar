#pragma once

#include "codegen.hpp"

namespace cinnabar {
  
struct CTypeInfo {
  std::string mangled_name;
  inline CTypeInfo(std::string_view mangled_name): mangled_name(mangled_name) {}
};

class CodegenC {

  const AST &ast;

  std::vector<CTypeInfo> ctypes;
  std::unordered_map<std::string_view, int> type_map;

  std::string out;
  
  void add_include(std::string_view file_name);
  
  void generate_types();

  [[noreturn]] void error(std::string_view message);

public:
  
  CodegenC(const AST &ast);
  
  std::string generate();

};


} // namespace cinnabar
