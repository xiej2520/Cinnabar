#include "codegen_c.hpp"

#include <functional>
#include <ranges>

namespace cinnabar {
  
CTypeInfo::CTypeInfo(std::string mangled_name): mangled_name(std::move(mangled_name)) {
}

CEnumInfo &CTypeInfo::enum_data() {
  return std::get<CEnumInfo>(data);
}
CStructInfo &CTypeInfo::struct_data() {
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

  std::vector<std::pair<const char *, const char *>> builtin_type_map = {
      {"__fun", ""},       {"unit", ""},        {"i8", "int8_t"},
      {"i16", "int16_t"},  {"i32", "int32_t"},  {"i64", "int64_t"},
      {"u8", "uint8_t"},   {"u16", "uint16_t"}, {"u32", "uint32_t"},
      {"u64", "uint64_t"}, {"f32", "float"},    {"f64", "double"},
      {"char", "char"},    {"bool", "bool"},    {"Ref", ""},
      {"VarRef", ""},      {"Span", ""},        {"VarSpan", ""},
      {"Span[char]", ""},
  };
  for (size_t i = 0; i < builtin_type_map.size(); i++) {
    ctypes.emplace_back(builtin_type_map[i].second);
    type_map[builtin_type_map[i].first] = i;
  }

  std::vector<int> dependencies(num_types, 0);
  std::vector<std::vector<int>> depends_on(num_types); // adj list

  // create dependency list and mangled type names
  for (int i = 0; i < num_types; i++) {
    //const TTypeInst &type = tast.types[i];
    // clang-format off
    /*
    std::visit(overload{
      [&](BuiltinType *) { },
      [&](EnumDecl *decl) {
        std::unordered_set<int> depend;

        CTypeInfo ctype(mangle_name(decl->name.str));

        for (auto &variant : decl->variants) {
          std::get<CEnumInfo>(ctype.data).variant_names.push_back(
              mangle_name(fmt::format("__{}_enum_var_{}", ctype.mangled_name, variant.name.str)));
          depend.insert(variant.);
        }
        ctypes.push_back(ctype);

        for (int type_id : depend) {
          depends_on[i].push_back(type_id);
        }
        dependencies[i] = depend.size();
        emit_type_forward_declare(i);
      },
      [&](StructDecl *decl) {
        std::unordered_set<int> depend;
        for (auto &field : decl->fields) {
          depend.insert(field.type);
        }
        for (int type_id : depend) {
          depends_on[i].push_back(type_id);
        }

        CTypeInfo ctype(mangle_name(decl->name.str));
        ctypes.push_back(ctype);
        emit_type_forward_declare(i);
      },
    }, type.decl);
    */
    // clang-format on
  }

  /*
  std::vector<TypeId> type_order;
  enum class Status {
    DONE,
    UNVISITED,
    CURRENT_VISIT,
  };
  std::vector<Status> visit(tast.types.size(), Status::UNVISITED);
  std::vector<int> current_search;

  std::function<void(TypeId)> dfs;
  dfs = [&](TypeId id) {
    if (visit[id] == Status::DONE) {
      return; // already in topo sort
    }
    if (visit[id] == Status::CURRENT_VISIT) { // loop detected
      std::string err_msg("Mutually recursive types: ");
      for (TypeId i : current_search) {
        err_msg += ast.types[i].name();
        err_msg += " -> ";
      }
      err_msg += ast.types[id].name();
      error(err_msg);
    }
    current_search.push_back(id);

    visit[id] = Status::CURRENT_VISIT;
    for (TypeId depend : depends_on[id]) {
      dfs(depend);
    }
    current_search.clear();
    type_order.push_back(id);
    visit[id] = Status::DONE;
  };

  for (size_t i = 0; i < ast.types.size(); i++) {
    dfs(i);
  }
  
  for (TypeId i : type_order) {
    std::visit(overload{
      [](BuiltinType *) {},
      [&](EnumDecl *) { emit_enum_definition(i); },
      [&](StructDecl *) { emit_struct_definition(i); },
    }, ast.types[i].decl);
  }
  */
}

void CodegenC::emit_type_forward_declare(TypeId id) {
  // clang-format off
  (void) id;
  /*
  std::visit(overload{
    [&](BuiltinType *) {}, // skip
    [&](EnumDecl *) {
      emit_line("typedef struct {0} {0};", ctypes[id].mangled_name);
    },
    [&](StructDecl *) {
      emit_line("typedef struct {0} {0};", ctypes[id].mangled_name);
    },
  }, ast.types[id].decl);
  */
  // clang-format on
}

void CodegenC::emit_function_foward_declare(FunId id) {
  std::string declaration;
  (void) declaration;
  (void) id;
  /*
  const FunInst &fun = ast.functions[id];
  if (fun.return_type == ast.builtin_type_map.at("unit")) {
    declaration += "void";
  }
  else {
    declaration += ast.types[fun.return_type].name();
  }
  declaration += ' ';
  declaration += cfuns[id].mangled_name;
  declaration += '(';
  if (fun.param_types.size() != 0) {
    for (size_t i = 0; i < fun.param_types.size(); i++) {
      declaration += fmt::format("{} {}, ",
          ast.types[fun.param_types[i]].name(), fun.decl->params[i]->name.str);
    }
    declaration.resize(declaration.size() - 2);
  }
  declaration += ");";

  emit_line("{}", declaration);
  */
}

void CodegenC::emit_enum_definition(TypeId id) {
  (void) id;
  /*
  const EnumDecl &decl = *std::get<EnumDecl *>(ast.types[id].decl);
  emit_line("struct {} {{", ctypes[id].mangled_name);
  // emit enum of variant
  emit_line("  enum {{");
  for (size_t j = 0; j < decl.variants.size(); j++) {
    emit_line("    {},", ctypes[id].enum_data().variant_names[j]);
  }
  emit_line("  }} {}__enum;", ctypes[id].mangled_name);
  // emit union of variant
  emit_line("  union {{");
  for (const auto &variant : decl.variants) {
    if (variant.type == type_map["unit"]) {
      //emit_line("    {} {};", variant.name.str);
    }
    else {
      emit_line("    {} {};", ctypes[variant.type].mangled_name, variant.name.str);
    }
  }
  emit_line("  }} {}__union;", ctypes[id].mangled_name);
  emit_line("}};");
  */
}

void CodegenC::emit_struct_definition(TypeId id) {
  (void) id;
  /*
  const StructDecl &decl = *std::get<StructDecl *>(ast.types[id].decl);
  emit_line("struct {} {{", ctypes[id].mangled_name);
  for (const auto &field : decl.fields) {
    emit_line("  {} {};", ctypes[field.type].mangled_name, field.name.str);
  }
  emit_line("}};");
  */
}

void CodegenC::emit_function_definition(FunId id) {
  (void) id;
}

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
