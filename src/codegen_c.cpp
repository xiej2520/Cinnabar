#include "codegen_c.hpp"

#include <functional>
#include <ranges>
#include <unordered_map>

namespace cinnabar {

CTypeInfo::CTypeInfo(std::string mangled_name)
    : mangled_name(std::move(mangled_name)), data(CBuiltinInfo{}) {}

const CEnumInfo &CTypeInfo::enum_info() const {
  return std::get<CEnumInfo>(data);
}
const CStructInfo &CTypeInfo::struct_info() const {
  return std::get<CStructInfo>(data);
}

CodegenC::CodegenC(const TAST &tast) : tast(tast) {}

std::string CodegenC::mangle_name(std::string_view name) {
  std::string res;
  for (char c : name) {
    if (c == '_' || isalnum(c)) {
      res += c;
    } else {
      res += '_';
    }
  }
  if (!used_names.contains(res)) {
    used_names.insert(res);
    return res;
  }
  for (int i = 0;; i++) {
    std::string res_concat = res + std::to_string(i);
    if (!used_names.contains(res_concat)) {
      used_names.insert(res_concat);
      return res_concat;
    }
  }
}

template <typename... Args>
void CodegenC::emit_line(fmt::format_string<Args...> fmt, Args &&...args) {
  out += fmt::vformat(fmt, fmt::make_format_args(std::forward<Args>(args)...));
  out += '\n';
}

void CodegenC::emit_include(std::string_view file_name) {
  emit_line("#include {}", file_name);
}

void CodegenC::emit_types() {
  int num_types = tast.types.size();
  ctypes.reserve(num_types);

  std::unordered_map<std::string_view, std::string> builtin_type_map = {
      {"__fun", ""},       {"unit", ""},        {"i8", "int8_t"},
      {"i16", "int16_t"},  {"i32", "int32_t"},  {"i64", "int64_t"},
      {"u8", "uint8_t"},   {"u16", "uint16_t"}, {"u32", "uint32_t"},
      {"u64", "uint64_t"}, {"f32", "float"},    {"f64", "double"},
      {"char", "char"},    {"bool", "bool"},    {"Ref", ""},
      {"VarRef", ""},      {"Span", ""},        {"VarSpan", ""},
      {"Span[char]", ""},
  };

  // generate CTypes and emit forward declares
  for (int i = 0; i < num_types; i++) {
    const TTypeInst &type = tast.types[i];
    // clang-format off
    std::visit(overload{
      [&](const BuiltinType &bt) {
        ctypes.push_back(CTypeInfo(builtin_type_map[bt.name.str]));
        type_map[bt.name.str] = i;
      },
      [&](const TEnumInst &inst) {
        std::unordered_set<int> depend;

        CTypeInfo ctype(mangle_name(inst.concrete_type.to_string()));
        CEnumInfo enum_info;

        for (auto &variant : inst.variants) {
          enum_info.variant_names.push_back(
              mangle_name(fmt::format("__{}_enum_var_{}", ctype.mangled_name, variant.first)));
        }
        ctype.data = std::move(enum_info);
        ctypes.push_back(ctype);

        emit_type_forward_declare(ctype.mangled_name);
      },
      [&](const TStructInst &inst) {
        std::unordered_set<int> depend;

        CTypeInfo ctype(mangle_name(inst.concrete_type.to_string()));
        ctypes.push_back(ctype);
        emit_type_forward_declare(ctype.mangled_name);
      },
    }, type.def);
    // clang-format on
  }

  for (int i = 0; i < num_types; i++) {
    // clang-format off
    std::visit(overload{
      [&](const CBuiltinInfo &) { },
      [&](const CEnumInfo &) {
        emit_enum_definition(i);
      },
      [&](const CStructInfo &) {
        emit_struct_definition(i);
      },
    }, ctypes[i].data);
    // clang-format on
  }
}

void CodegenC::emit_type_forward_declare(std::string_view mangled_name) {
  emit_line("typedef struct {0} {0};", mangled_name);
}

void CodegenC::emit_function_foward_declare(FunId id) {
  std::string declaration;
  const TFunInst &inst = tast.functions[id];
  if (inst.return_type == tast.builtin_type_map.at("unit")) {
    declaration += "void";
  } else {
    declaration += ctypes[inst.return_type].mangled_name;
  }
  declaration += ' ';
  declaration += cfuns[id].mangled_name;
  declaration += '(';
  if (inst.params.size() != 0) {
    for (size_t i = 0; i < inst.params.size(); i++) {
      declaration += fmt::format(
          "{} {}, ", tast.types[inst.params[i]->type].name(),
          inst.params[i]->name.str
      );
    }
    declaration.resize(declaration.size() - 2);
  }
  declaration += ");";

  emit_line("{};", declaration);
}

void CodegenC::emit_enum_definition(TypeId id) {
  const CTypeInfo &ctype = ctypes[id];
  const CEnumInfo &enum_info = ctype.enum_info();
  const TEnumInst &inst = std::get<TEnumInst>(tast.types[id].def);
  emit_line("struct {} {{", ctype.mangled_name);
  // emit enum of variant
  emit_line("  enum {{");
  for (size_t j = 0; j < inst.variants.size(); j++) {
    emit_line("    {},", enum_info.variant_names[j]);
  }
  emit_line("  }} {}__enum;", ctype.mangled_name);
  // emit union of variant
  emit_line("  union {{");
  for (const auto &variant : inst.variants) {
    if (variant.second.first == type_map["unit"]) {
    } else {
      TypeId variant_id = variant.second.first;
      emit_line("    {} {};", ctypes[variant_id].mangled_name, variant.first);
    }
  }
  emit_line("  }} {}__union;", ctypes[id].mangled_name);
  emit_line("}};");
}

void CodegenC::emit_struct_definition(TypeId id) {
  const CTypeInfo &ctype = ctypes[id];
  const TStructInst &inst = std::get<TStructInst>(tast.types[id].def);

  emit_line("struct {} {{", ctype.mangled_name);
  for (const auto &field : inst.fields) {
    emit_line("  {} {};", ctypes[field.second.first].mangled_name, field.first);
  }
  emit_line("}};");
}

void CodegenC::emit_function_definition(FunId id) { (void)id; }

std::string CodegenC::generate() {
  out = "";
  emit_include("<stdint.h>");
  emit_types();

  return out;
}

[[noreturn]] void CodegenC::error(std::string_view message) {
  fmt::print(stderr, "C Code Generation Error: {}\n", message);
  abort();
}

} // namespace cinnabar
