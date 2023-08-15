#include "codegen_c.hpp"

#include <functional>

namespace cinnabar {

CodegenC::CodegenC(const AST &ast) : ast(ast) {}

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
  int num_types = ast.types.size();
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
  std::vector<std::vector<int>> types_depending_on(num_types); // adj list

  // create dependency list and mangled type names
  for (int i = 0; i < num_types; i++) {
    const Type &type = ast.types[i];
    // clang-format off
    std::visit(overload{
      [&](BuiltinType *) { },
      [&](EnumDecl *decl) {
        std::unordered_set<int> depend;
        for (auto &variant : decl->variants) {
          depend.insert(variant.type);
        }
        for (int type_id : depend) {
          types_depending_on[type_id].push_back(i);
        }
        dependencies[i] = depend.size();

        CTypeInfo ctype(mangle_name(decl->name.str));
        ctypes.push_back(ctype);
        emit_type_forward_declare(i);
      },
      [&](StructDecl *decl) {
        std::unordered_set<int> depend;
        for (auto &field : decl->fields) {
          depend.insert(field.type);
        }
        for (int type_id : depend) {
          types_depending_on[type_id].push_back(i);
        }

        CTypeInfo ctype(mangle_name(decl->name.str));
        ctypes.push_back(ctype);
        emit_type_forward_declare(i);
      },
    }, type.type_decl_ptr);
    // clang-format on
  }

  std::vector<TypeId> type_order;
  enum class Status {
    DONE,
    UNVISITED,
    CURRENT_VISIT,
  };
  std::vector<Status> visit(ast.types.size(), Status::UNVISITED);

  std::function<void(TypeId)> dfs;
  dfs = [&](TypeId id) {
    if (visit[id] == Status::DONE) {
      return; // already in topo sort
    }
    if (visit[id] == Status::CURRENT_VISIT) { // loop detected
      std::string err_msg("Mutually recursive types: ");
      err_msg += ast.types[id].name(); // current

      size_t tn = type_order.size();
      for (size_t i = 0; i < tn && type_order[tn - 1 - i] != id; i++) {
        err_msg += " <- ";
        err_msg += ast.types[type_order[type_order.size() - 1 - i]].name();
      }
      err_msg += fmt::format(" <- {}", ast.types[id].name());
      error(err_msg);
    }

    visit[id] = Status::CURRENT_VISIT;
    type_order.push_back(id);
    for (TypeId depend : types_depending_on[id]) {
      dfs(depend);
    }
    visit[id] = Status::DONE;
  };

  for (size_t i = 0; i < ast.types.size(); i++) {
    dfs(i);
  }
  reverse(type_order.begin(), type_order.end());
  
  for (TypeId i : type_order) {
    std::visit(overload{
      [](BuiltinType *) {},
      [&](EnumDecl *) { emit_enum_definition(i); },
      [&](StructDecl *) { emit_struct_definition(i); },
    }, ast.types[i].type_decl_ptr);
  }
}

void CodegenC::emit_type_forward_declare(TypeId i) {
  std::visit(
      overload{
          [&](BuiltinType *) {}, // skip
          [&](EnumDecl *) {
            emit_line("typedef struct {0} {0};", ctypes[i].mangled_name);
          },
          [&](StructDecl *) {
            emit_line("typedef struct {0} {0};", ctypes[i].mangled_name);
          },
      },
      ast.types[i].type_decl_ptr
  );
}

void CodegenC::emit_enum_definition(TypeId i) {
  const EnumDecl &decl = *std::get<EnumDecl *>(ast.types[i].type_decl_ptr);
  emit_line("struct {} {{", ctypes[i].mangled_name);
  // emit enum of variant
  emit_line("  enum {{");
  for (const auto &variant : decl.variants) {
    emit_line("    __{}__enum_var__{},", ctypes[i].mangled_name, variant.name.str);
  }
  emit_line("  }}");
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
  emit_line("  }}");
  emit_line("}}", ctypes[i].mangled_name);
}

void CodegenC::emit_struct_definition(TypeId i) {
  const StructDecl &decl = *std::get<StructDecl *>(ast.types[i].type_decl_ptr);
  emit_line("struct {} {{", ctypes[i].mangled_name);
  for (const auto &field : decl.fields) {
    emit_line("  {} {};", ctypes[field.type].mangled_name, field.name.str);
  }
  emit_line("}}", ctypes[i].mangled_name);
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
