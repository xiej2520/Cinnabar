#include "codegen_c.hpp"

#include <functional>
#include <ranges>
#include <unordered_map>

namespace cinnabar {

CTypeInfo::CTypeInfo(std::string mangled_name)
    : mangled_name(std::move(mangled_name)), data(CBuiltinInfo{}) {}

CFunInfo::CFunInfo(std::string mangled_name)
    : mangled_name(std::move(mangled_name)) {}

const CEnumInfo &CTypeInfo::enum_info() const {
  return std::get<CEnumInfo>(data);
}
const CStructInfo &CTypeInfo::struct_info() const {
  return std::get<CStructInfo>(data);
}

CodegenC::CodegenC(const TAST &tast) : tast(tast) {}

std::string CodegenC::mangle_name(std::string_view name) {
  // C and C++ reserve:
  //   identifiers that begin with an underscore followed by an uppercase letter
  //   identifiers in the global namespace that begin with an underscore
  // C reserves names beginning with double underscore, C++ reserves names with
  // a double underscore anywhere
  std::string res;
  if (name.empty() || name.front() == '_') {
    res += "cb";
  }
  for (const char c : name) {
    if (static_cast<bool>(isalnum(c))) {
      res += c;
    } else if (res.back() == '_') {
      // no double underscores
      res += 'U';
    } else {
      res += '_';
    }
  }
  if (!used_names.contains(res)) {
    used_names.insert(res);
    return res;
  }
  for (int i = 0;; i++) {
    auto res_concat = fmt::format("{}{}", res, i);
    if (!used_names.contains(res_concat)) {
      used_names.insert(res_concat);
      return res_concat;
    }
  }
}

std::string_view CodegenC::type_name(TypeId id) {
  return std::string_view(ctypes[id].mangled_name);
}

template <typename... Args>
void CodegenC::emit(fmt::format_string<Args...> fmt, Args &&...args) {
  out += fmt::vformat(fmt, fmt::make_format_args(std::forward<Args>(args)...));
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
  size_t num_types = tast.types.size();
  ctypes.reserve(num_types);

  std::unordered_map<std::string_view, std::string> primitive_map = {
      {"__fun", ""},       {"unit", ""},        {"i8", "int8_t"},
      {"i16", "int16_t"},  {"i32", "int32_t"},  {"i64", "int64_t"},
      {"u8", "uint8_t"},   {"u16", "uint16_t"}, {"u32", "uint32_t"},
      {"u64", "uint64_t"}, {"f32", "float"},    {"f64", "double"},
      {"char", "char"},    {"bool", "bool"},    {"isize", "ssize_t"},
      {"usize", "size_t"},
  };

  // generate CTypes and emit forward declares
  for (size_t i = 0; i < num_types; i++) {
    const TTypeInst &type = tast.types[i];
    // clang-format off
    std::visit(overload{
      [&](const Primitive &p) {
        CTypeInfo ctype(primitive_map.at(p.name.str));
        ctype.data = CPrimitiveInfo{};
        ctypes.push_back(ctype);;
      },
      [&](const TBuiltinType &bt) {
        CTypeInfo ctype(mangle_name(bt.concrete_type.to_string()));
        ctype.data = CBuiltinInfo{};
        ctypes.push_back(ctype);;
        emit_type_forward_declare(ctype.mangled_name);
      },
      [&](const TEnumInst &inst) {
        CTypeInfo ctype(mangle_name(inst.concrete_type.to_string()));
        CEnumInfo enum_info;
        enum_info.enum_mangled_name = mangle_name(ctype.mangled_name + "_enum");
        enum_info.union_mangled_name = mangle_name(ctype.mangled_name + "_union");

        enum_info.variant_names.resize(inst.variants.size());
        for (const auto &variant : inst.variants) {
          enum_info.variant_names[variant.second.second] = {
              mangle_name(fmt::format("_{}_enum_var_{}", ctype.mangled_name, variant.first)),
              variant.second.first,
              std::string(variant.first)
              };
        }
        ctype.data = std::move(enum_info);
        ctypes.push_back(ctype);

        emit_type_forward_declare(ctype.mangled_name);
      },
      [&](const TStructInst &inst) {
        CTypeInfo ctype(mangle_name(inst.concrete_type.to_string()));
        CStructInfo struct_info;

        for (auto &field : inst.fields) {
          struct_info.field_names.push_back(std::string(field.first));
        }
        ctype.data = std::move(struct_info);
        ctypes.push_back(ctype);
        emit_type_forward_declare(ctype.mangled_name);
      },
    }, type.def);
    // clang-format on
  }

  for (size_t i = 0; i < num_types; i++) {
    // clang-format off
    std::visit(overload{
      [&](const CPrimitiveInfo &) { },
      [&](const CBuiltinInfo &) {
        emit_builtin_definition(i);
      },
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

void CodegenC::emit_builtin_definition(TypeId id) {
  const CTypeInfo &ctype = ctypes[id];
  const TBuiltinType &bt = std::get<TBuiltinType>(tast.types[id].def);
  emit_line("struct {} {{", ctype.mangled_name);
  switch (bt.args.index()) {
    case TBuiltinEnum::Ref: {
      TypeId arg = std::get<TBuiltinEnum::Ref>(bt.args);
      emit_line("  const {} *ptr;", ctypes[arg].mangled_name);
    }
    break;
    case TBuiltinEnum::VarRef: {
      TypeId arg = std::get<TBuiltinEnum::VarRef>(bt.args);
      emit_line("  {} *ptr;", ctypes[arg].mangled_name);
    }
    break;
    case TBuiltinEnum::Span: {
      TypeId arg = std::get<TBuiltinEnum::Span>(bt.args);
      emit_line("  const {} *ptr;", ctypes[arg].mangled_name);
      emit_line("  size_t size;");
    }
    break;
    case TBuiltinEnum::VarSpan: {
      TypeId arg = std::get<TBuiltinEnum::VarSpan>(bt.args);
      emit_line("  {} *ptr;", ctypes[arg].mangled_name);
      emit_line("  size_t size;");
    }
    break;
    case TBuiltinEnum::Array: {
      auto [type_id, size] = std::get<TBuiltinEnum::Array>(bt.args);
      emit_line("  {} data[{}];", ctypes[type_id].mangled_name, size);
    }
    break;
  }
  emit_line("}};");
}

void CodegenC::emit_enum_definition(TypeId id) {
  const CTypeInfo &ctype = ctypes[id];
  const CEnumInfo &enum_info = ctype.enum_info();
  emit_line("struct {} {{", ctype.mangled_name);
  // emit enum of variant
  emit_line("  enum {{");
  for (const auto &variant : enum_info.variant_names) {
    emit_line("    {},", variant.enum_variant_name);
  }
  emit_line("  }} {};", enum_info.enum_mangled_name);
  // emit union of variant
  emit_line("  union {{");
  for (const auto &variant : enum_info.variant_names) {
    if (variant.type == tast.primitive_map.at("unit")) {
    } else {
      emit_line(
          "    {} {};", ctypes[variant.type].mangled_name,
          variant.union_variant_name
      );
    }
  }
  emit_line("  }} {};", enum_info.union_mangled_name);
  emit_line("}};");
}

void CodegenC::emit_struct_definition(TypeId id) {
  const CTypeInfo &ctype = ctypes[id];
  const CStructInfo &struct_info = ctype.struct_info();
  const TStructInst &inst = std::get<TStructInst>(tast.types[id].def);

  emit_line("struct {} {{", ctype.mangled_name);
  size_t num_fields = inst.fields.size();
  std::vector<TypeId> field_type(num_fields);
  for (const auto &field : inst.fields) {
    field_type[field.second.second] = field.second.first;
  }
  for (size_t i = 0; i < num_fields; i++) {
    emit_line(
        "  {} {};", ctypes[field_type[i]].mangled_name,
        struct_info.field_names[i]
    );
  }
  emit_line("}};");
}

void CodegenC::emit_functions() {
  size_t num_funs = tast.functions.size();
  for (size_t i = 0; i < num_funs; i++) {
    CFunInfo cfun(mangle_name(tast.functions[i].name()));
    cfuns.push_back(std::move(cfun));
    emit_function_forward_declare(i);
  }

  for (size_t i = 0; i < num_funs; i++) {
    emit_function_definition(i);
  }
}

void CodegenC::emit_function_signature(FunId id) {
  std::string declaration;
  const TFunInst &inst = tast.functions[id];
  if (inst.return_type == tast.primitive_map.at("unit")) {
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
          "{} {}, ", ctypes[inst.params[i]->type].mangled_name,
          inst.params[i]->name.str
      );
    }
    declaration.resize(declaration.size() - 2);
  } else {
    declaration += "void";
  }
  declaration += ")";
  emit("{}", declaration);
}

void CodegenC::emit_function_forward_declare(FunId id) {
  emit_function_signature(id);
  emit_line(";");
}

void CodegenC::emit_function_definition(FunId id) {
  const TFunInst &inst = tast.functions[id];
  emit_function_signature(id);
  emit_line(" {{");
  // buffer_stack.emplace_back();
  for (auto &stmt : inst.body->stmts) {
    emit_stmt(stmt);
  }
  emit_line("}}");
}

void CodegenC::emit_stmt(const TStmt &stmt) {
  //// first traverse, find temporary assignment targets
  // preprocess(stmt);
  std::string loc;
  // clang-format off
  std::visit(overload{
    [&](const std::unique_ptr<TAssign> &stmt) {
      loc += emit_expr(stmt->lhs);
      loc += ' ';
      loc += to_string(stmt->op);
      loc += ' ';
      loc += emit_expr(stmt->rhs);
    },
    [&](const std::unique_ptr<TBreak> &) { emit_line("break"); },
    [&](const std::unique_ptr<TContinue> &) { emit_line("continue"); },
    [&](const TExpr &expr) {
      loc += emit_expr(expr);
    },
    [&](const std::unique_ptr<TFor> &) { },
    [&](const std::unique_ptr<TReturn> &stmt) {
      loc += "return";
      if (stmt->value.has_value()) {
        loc += " ";
        loc += emit_expr(stmt->value.value());
      }
    },
    [&](const std::unique_ptr<TVarInst> &stmt) {
      auto mangled_type = type_name(stmt->type);
      loc += mangled_type;
      loc += ' ';
      loc += stmt->name.str;
      if (stmt->initializer.has_value()) {
        loc += " = ";
        loc += emit_expr(stmt->initializer.value());
      }
    },
    [&](const std::unique_ptr<TWhile> &) { },
  }, stmt.node);

  loc += ';';
  emit_line("{}", loc);
  // clang-format on
}

std::string CodegenC::emit_expr(const TExpr &expr) {
  // clang-format off
  return std::visit(overload{
    [&](std::monostate) { return std::string{}; },
    [&](const std::unique_ptr<TBinary> &expr) {
      return fmt::format("({} {} {})",
          emit_expr(expr->left),
          to_string(expr->op),
          emit_expr(expr->right));
    },
    [&](const std::unique_ptr<TBlock> &block) -> std::string { 
      if (block->type == tast.primitive_map.at("unit")) {
        for (auto &stmt : block->stmts) {
          emit_stmt(stmt);
        }
      }
      else {
        auto block_var_name = mangle_name("_cb_block_expr_val");
        emit_line("{} {};", ctypes[block->type].mangled_name, block_var_name);
        emit_line("{{");
        for (size_t i = 0; i + 1 < block->stmts.size(); i++) {
          emit_stmt(block->stmts[i]);
        }
        auto expr_val = emit_expr(block->stmts.back().as<TExpr>());
        emit_line("{} = {};", block_var_name, expr_val);
        emit_line("}}");
        return block_var_name;
      }
      
      return std::string{};
    },
    [&](const std::unique_ptr<TDotRef> &expr) {
      if (expr->left.type() != tast.primitive_map.at("__fun")) {
        CTypeInfo &type_info = ctypes[expr->left.type()];
        auto res = "(" + emit_expr(expr->left) + ").";
        if (CStructInfo *struct_info = std::get_if<CStructInfo>(&type_info.data)) {
          res += struct_info->field_names[expr->prop_idx];
        }
        else if (CEnumInfo *enum_info = std::get_if<CEnumInfo>(&type_info.data)) {
          res += enum_info->union_mangled_name + "." + enum_info->variant_names[expr->prop_idx].union_variant_name;
        }
        else {
          error("Cannot take dot ref of builtin type.");
        }
        return res;
      }
      error("Cannot take dot ref of a function.");
    },
    [&](const std::unique_ptr<TFunCall> &expr) {
      auto res = emit_expr(expr->callee);
      res += '(';
      if (!expr->args.empty()) {
        for (auto &arg : expr->args) {
          res += emit_expr(arg);
          res += ", ";
        }
        res.pop_back();
        res.pop_back();
      }
      res += ')';
      return res;
    },
    [&](const std::unique_ptr<TIf> &expr) {
      // does not have value
      if (expr->type == tast.primitive_map.at("unit")) {
        auto expr_condition = emit_expr(expr->branches.front()->condition);
        emit_line("if ({}) {{", expr_condition);
        for (auto &stmt : expr->branches.front()->block->stmts) {
          emit_stmt(stmt);
        }
        emit_line("}}");
        // use nested else because we need to evaluate the expression, which may
        // be a block
        for (size_t i = 1; i < expr->branches.size(); i++) {
          emit_line("else {{");
          auto expr_condition = emit_expr(expr->branches[i]->condition);

          if (i != expr->branches.size() - 1 || !expr->has_else()) {
            emit_line("if ({}) {{", expr_condition);
            for (auto &stmt : expr->branches[i]->block->stmts) {
              emit_stmt(stmt);
            }
            // closing brace for if
            emit_line("}}");
          }
          else { // is else
            for (auto &stmt : expr->branches[i]->block->stmts) {
              emit_stmt(stmt);
            }
          }

        }
        // closing brace for else
        for (size_t i = 1; i < expr->branches.size(); i++) {
          emit_line("}}");
        }
        return std::string{};
      }

      // if has a value
      auto if_var_name = mangle_name("_cb_if_expr_val");
      // if condition
      emit_line("{} {};", ctypes[expr->type].mangled_name, if_var_name);
      auto expr_condition = emit_expr(expr->branches.front()->condition);
      emit_line("if ({}) {{", expr_condition);
      // statements in body
      auto &block = expr->branches.front()->block;
      for (size_t i = 0; i + 1 < block->stmts.size(); i++) {
        emit_stmt(block->stmts[i]);
      }
      // final statement
      auto expr_val = emit_expr(block->stmts.back().as<TExpr>());
      emit_line("{} = {};", if_var_name, expr_val);
      emit_line("}}");

      // use nested else because we need to evaluate the expression, which may
      // be a block
      for (size_t i = 1; i < expr->branches.size(); i++) {
        emit_line("else {{");
        auto expr_condition = emit_expr(expr->branches[i]->condition);
        if (i != expr->branches.size() - 1 || !expr->has_else()) {
          emit_line("if ({}) {{", expr_condition);
        }

        auto &block = expr->branches[i]->block;
        for (size_t i = 0; i + 1 < block->stmts.size(); i++) {
          emit_stmt(block->stmts[i]);
        }
        auto expr_val = emit_expr(block->stmts.back().as<TExpr>());
        emit_line("{} = {};", if_var_name, expr_val);

        if (i != expr->branches.size() - 1 || !expr->has_else()) {
          emit_line("}}");
        }
      }
      for (size_t i = 1; i < expr->branches.size(); i++) {
        // closing brace for else
        emit_line("}}");
      }

      return if_var_name;
    },
    [&](const std::unique_ptr<TLiteral> &expr) {
      return std::visit(overload{
        [&](int32_t val) { return fmt::format("{}", val); },
        [&](int64_t val) { return fmt::format("{}", val); },
        [&](float val) { return fmt::format("{}", val); },
        [&](double val) { return fmt::format("{}", val); },
        [&](bool val) -> std::string { return val ? "true" : "false"; },
        [&](char val) { return fmt::format("'{}'", val); },
        [&](const std::string &val) { return fmt::format("\"{}\"", val); },
      }, expr->val);
    },
    [&](const std::unique_ptr<TUnary> &expr) {
      auto expr_operand = emit_expr(expr->operand);
      return fmt::format("({}{})", to_string(expr->op), expr_operand);
    },
    [&](const std::unique_ptr<TVariable> &expr) { return std::string(expr->name.str); },
  }, expr.node);
  // clang-format on
}

std::string CodegenC::generate() {
  out = "";
  emit_include("<stdint.h>");
  emit_include("<stdbool.h>");
  emit_types();
  emit_functions();

  return out;
}

[[noreturn]] void CodegenC::error(std::string_view message) {
  fmt::print(stderr, "C Code Generation Error: {}\n", message);
  abort();
}

} // namespace cinnabar
