#include "typecheck_resolver.hpp"

#include "fmt/core.h"

namespace cinnabar {

TypeResolver::TypeResolver(const AST &ast) : ast(ast) {}

std::vector<std::string> default_builtin_types = {
    "i8",         "i16", "i32",    "i64",   "u8",       "u16",
    "u32",        "u64", "f32",    "f64",   "char",     "bool",
    "unit",       "Ref", "VarRef", "Slice", "VarSlice",

    "Slice[char]" // String for now
};

TypeId TypeResolver::find_type(std::string_view name) {
  for (int i = namespaces.size() - 1; i >= 0; i--) {
    if (namespaces[i]->type_decls.contains(name)) {
      return namespaces[i]->type_decls[name];
    }
  }
  fmt::print(stderr, "Could not find type {}", name);
  abort();
}

TypeId TypeResolver::find_binary_op(BinaryOp op, TypeId lhs_type,
                                    TypeId rhs_type) {
  fmt::print(stderr, "Invalid binary operation: {} {} {}\n", to_string(op),
             lhs_type, rhs_type);
  abort();
}


void TypeResolver::resolve() {
  /*
  std::vector<TDeclaration> decls;

  TNamespace globals = reserve_names(ast.globals);
  namespaces.push_back(&globals);

  std::vector<std::unique_ptr<TBuiltin>> builtins;
  tast_types = std::make_unique<std::vector<Type>>();

  for (auto name : default_builtin_types) {
    if (ast.globals.names.contains(name)) {
      fmt::print(stderr, "Globals contained name '{}' matching builtin.\n",
                 name);
      abort();
    }
    builtins.emplace_back(std::make_unique<TBuiltin>(name));
    TypeId t = reserve_type();
    link_type(t, builtins.back().get());

    globals.link_type(builtins.back()->name, t);
  }

  for (const Declaration &decl : ast.decls) {
    decls.push_back(resolve(decl));
  }

  TAST tast(std::move(decls), std::move(builtins), std::move(*tast_types),
            globals);
  return tast;
  */
}

} // namespace cinnabar
