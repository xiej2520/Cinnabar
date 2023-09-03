#pragma once

#include "codegen.hpp"

#include "fmt/core.h"

namespace cinnabar {

struct CBuiltinInfo {

};

struct CEnumInfo {
  std::string enum_mangled_name;
  std::string union_mangled_name;
  struct VariantName {
    std::string enum_variant_name;
    TypeId type;
    std::string union_variant_name;
  };
  std::vector<VariantName> variant_names;
};

struct CStructInfo {
  std::vector<std::string> field_names;
};

struct CTypeInfo {
  std::string mangled_name;
  std::variant<CBuiltinInfo, CEnumInfo, CStructInfo> data;

  const CEnumInfo &enum_info() const;
  const CStructInfo &struct_info() const;

  CTypeInfo(std::string mangled_name);
};

struct CFunInfo {
  std::string mangled_name;

  CFunInfo(std::string mangled_name);
};

class CodegenC {

  const TAST &tast;

  std::vector<CTypeInfo> ctypes;
  std::vector<CFunInfo> cfuns;

  std::unordered_map<std::string_view, int> type_map;
  
  std::unordered_set<std::string> used_names; // easier to have owning

  std::string out;

  // takes valid identifier or Generic type, mangles it
  // Map[i32,Ref[i32]] -> "Map_i32_Ref_i32__4" ...
  std::string mangle_name(std::string_view name);
  
  std::string_view type_name(TypeId id);
  
  template<typename... Args>
  void emit(fmt::format_string<Args...> fmt, Args&&... args);
  template<typename... Args>
  void emit_line(fmt::format_string<Args...> fmt, Args&&... args);

  void emit_include(std::string_view file_name);
  void emit_types();
  void emit_type_forward_declare(std::string_view mangled_name);

  // check type before calling
  void emit_enum_definition(TypeId id);
  void emit_struct_definition(TypeId id);
  
  void emit_functions();
  void emit_function_signature(FunId id);
  void emit_function_forward_declare(FunId id);
  
  void emit_function_definition(FunId id);
  
  void emit_stmt(const TStmt &stmt);
  
  //// should probably be replaced with an IR stage
  //void preprocess(const TStmt &stmt);
  //void preprocess(const TExpr &expr);

  // emits the code needed to evaluate the expr, then return string for expr (in C)
  // postorder traversal?
  std::string emit_expr(const TExpr &expr);

  [[noreturn]] void error(std::string_view message);

public:
  
  CodegenC(const TAST &tast);
  
  std::string generate();

};


} // namespace cinnabar
