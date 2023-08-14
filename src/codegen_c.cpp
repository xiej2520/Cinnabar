#include "codegen_c.hpp"

#include <functional>
#include "fmt/core.h"

namespace cinnabar {

CodegenC::CodegenC(const AST &ast) : ast(ast) {}

void CodegenC::add_include(std::string_view file_name) {
  out += fmt::format("#include {}\n", file_name);
}

void CodegenC::generate_types() {
  int num_types = ast.types.size();
  ctypes.reserve(num_types);

  std::vector<std::pair<const char *, const char *>> builtin_type_map = {
      {"__fun", ""},      {"unit", ""},        {"i8", "int8_t"},
      {"i16", "int16_t"}, {"i32", "int32_t"},  {"i64", "int64_t"},
      {"u8", "uint8_t"},  {"u16", "uint16_t"}, {"u32", "u64"},
      {"f32", "float"},   {"f64", "double"},   {"char", "char"},
      {"bool", "bool"},   {"Ref", ""},         {"VarRef", ""},
      {"Span", ""},       {"VarSpan", ""},     {"Span[char]", ""},
  };
  for (size_t i = 0; i < builtin_type_map.size(); i++) {
    ctypes.emplace_back(builtin_type_map[i].second);
    type_map[builtin_type_map[i].first] = i;
  }

  std::vector<int> dependencies(num_types, 0);
  std::vector<std::vector<int>> types_depending_on(num_types); // adj list

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
      },
      [&](StructDecl *decl) {
        std::unordered_set<int> depend;
        for (auto &field : decl->fields) {
          depend.insert(field.type);
        }
        for (int type_id : depend) {
          types_depending_on[type_id].push_back(i);
        }
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
    if (visit[id] == Status::DONE) return; // already in topo sort
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
}

std::string CodegenC::generate() {
  out = "";
  add_include("<stdint.h>");
  (void)ast;

  generate_types();
  return out;
}

[[noreturn]] void CodegenC::error(std::string_view message) {
  fmt::print(stderr, "C Code Generation Error: {}\n", message);
  abort();
}

} // namespace cinnabar
