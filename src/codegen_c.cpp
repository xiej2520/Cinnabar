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
  //buffer_stack.emplace_back();
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
      (void) stmt;
    },
    [&](const std::unique_ptr<TBreak> &) { emit_line("break"); },
    [&](const std::unique_ptr<TContinue> &) { emit_line("continue"); },
    [&](const TExpr &expr) {
      loc += emit_expr(expr);
    },
    [&](const std::unique_ptr<TFor> &) { },
    [&](const std::unique_ptr<TReturn> &) { },
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
  //while (!temp_assign_targets.empty()) {
  //  fmt::print("{}\n", temp_assign_targets.front());
  //  temp_assign_targets.pop();
  //}
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
      if (block->type == tast.builtin_type_map.at("unit")) {
        for (auto &stmt : block->stmts) {
          emit_stmt(stmt);
        }
      }
      else {
        auto block_var_name = mangle_name("__cb_block_expr_val");
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
    [&](const std::unique_ptr<TDotRef> &) { return std::string{}; },
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
      if (expr->type == tast.builtin_type_map.at("unit")) {
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
      auto if_var_name = mangle_name("__cb_if_expr_val");
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
