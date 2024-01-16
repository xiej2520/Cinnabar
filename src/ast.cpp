#include "ast.hpp"

namespace cinnabar {

using std::unique_ptr;
using std::vector;

using enum Lexeme;

const std::vector<std::string_view> builtin_names = {
    "Unit", "i8",  "i16",  "i32",  "i64",   "u8",    "u16", "u32",   "u64",
    "f32",  "f64", "char", "bool", "isize", "usize", "Ref", "Array", "Span",
};

[[nodiscard]] bool GenericSignature::is_concrete() const {
  return params.empty();
}

GenericInst::GenericInst(Token base_name) : base_name(base_name) {}
GenericInst::GenericInst(Token base_name, vector<GenericArg> args)
    : base_name(base_name), args(std::move(args)){};

[[nodiscard]] std::string GenericInst::to_string() const {
  std::string res{base_name.str};
  if (!args.empty()) {
    res += '[';
    for (auto &child : args) {
      res += child.to_string();
      res += ',';
    }
    res.back() = ']';
  }
  return res;
}

[[nodiscard]] bool GenericInst::is_concrete() const { return args.empty(); }

[[nodiscard]] std::string GenericArg::to_string() const {
  return std::visit(
      overload{
          [](const GenericInst &inst) { return inst.to_string(); },
          [](const Literal &literal) {
            return cinnabar::to_string(literal.val);
          },
      },
      data
  );
}

[[nodiscard]] std::string GenericSignature::to_string() const {
  std::string res{base_name.str};
  if (!params.empty()) {
    res += '[';
    for (auto &child : params) {
      res += child.to_string();
      res += ',';
    }
    res.back() = ']';
  }
  return res;
}

[[nodiscard]] std::string GenericParam::to_string() const {
  std::string res{name.str};
  std::visit(
      overload{
          [](TypeParamData) {},
          [&](const ValueParamData &data) {
            res += ',';
            res += data.type.to_string();
          },
      },
      param
  );
  return res;
}

Namespace::Namespace(Namespace *parent) : parent(parent) {}

DeclPtr Namespace::get_name(std::string_view name) {
  if (names.contains(name)) {
    return names[name];
  } else if (parent == nullptr) {
    return static_cast<VarDecl *>(nullptr);
  }
  return parent->get_name(name);
}

std::string Namespace::to_string(int cur) {
  auto res = fmt::format("{:{}}Namespace: ", "", cur);
  std::vector<std::string_view> builtin_names;
  std::vector<std::string_view> type_names;
  std::vector<std::string_view> fun_names;
  std::vector<std::string_view> var_names;
  for (auto &p : names) {
    // clang-format off
    std::visit(overload{
      [&](BuiltinType) { type_names.push_back(p.first); },
      [&](StructDecl *) { type_names.push_back(p.first); },
      [&](EnumDecl *) { type_names.push_back(p.first); },
      [&](FunDecl *) { fun_names.push_back(p.first); },
      [&](VarDecl *) { var_names.push_back(p.first); },
    }, p.second);
    // clang-format on
  }
  if (!type_names.empty()) {
    res.append("Types: ");
    for (auto &name : type_names) {
      res.append(fmt::format("{}, ", name));
    }
  }
  if (!fun_names.empty()) {
    res.append("Functions: ");
    for (auto &name : fun_names) {
      res.append(fmt::format("{}, ", name));
    }
  }
  if (!var_names.empty()) {
    res.append("Variables: ");
    for (auto &name : var_names) {
      res.append(fmt::format("{}, ", name));
    }
  }
  res.pop_back();
  res.back() = '\n';
  return res;
}

// statements
Assign::Assign(AssignOp op, Expr lhs, Expr rhs)
    : op(op), lhs(std::move(lhs)), rhs(std::move(rhs)) {}

Declaration::Declaration(DeclVariant decl) : decl(std::move(decl)) {}

// declaration statements
EnumDecl::EnumDecl(
    GenericSignature name_params,
    vector<std::pair<Token, GenericInst>> variants,
    vector<unique_ptr<FunDecl>> methods, unique_ptr<Namespace> namesp
)
    : name_params(std::move(name_params)), variants(variants),
      methods(std::move(methods)), namesp(std::move(namesp)) {}

FunDecl::FunDecl(
    GenericSignature name_params, vector<unique_ptr<VarDecl>> params,
    GenericInst return_type, unique_ptr<Block> body
)
    : name_params(std::move(name_params)), params(std::move(params)),
      return_type(std::move(return_type)), body(std::move(body)) {}

StructDecl::StructDecl(
    GenericSignature name_params, vector<std::pair<Token, GenericInst>> fields,
    vector<unique_ptr<FunDecl>> methods, unique_ptr<Namespace> namesp
)
    : name_params(std::move(name_params)), fields(fields),
      methods(std::move(methods)), namesp(std::move(namesp)) {}

VarDecl::VarDecl(
    Token name, std::optional<GenericInst> type_specifier,
    std::optional<Expr> initializer
)
    : name(name), type_specifier(type_specifier),
      initializer(std::move(initializer)) {}

// expressions
Expression::Expression(Expr expr) : expr(std::move(expr)) {}

Binary::Binary(BinaryOp op, Expr left, Expr right)
    : op(op), left(std::move(left)), right(std::move(right)) {}

Block::Block(vector<Stmt> stmts, unique_ptr<Namespace> namesp)
    : stmts(std::move(stmts)), namesp(std::move(namesp)) {}

DotRef::DotRef(Expr lvalue, Token name)
    : lvalue(std::move(lvalue)), name(name) {}

FunCall::FunCall(Expr callee, vector<Expr> args)
    : callee(std::move(callee)), args(std::move(args)) {}

If::If(vector<std::unique_ptr<Branch>> branches)
    : branches(std::move(branches)) {}

If::Branch::Branch(Expr condition, unique_ptr<Block> block)
    : condition(std::move(condition)), block(std::move(block)) {}

Literal::Literal(LiteralVariant val) : val(val) {}

NamedValue::NamedValue(Token name) : name(name) {}

Unary::Unary(UnaryOp op, Expr operand) : op(op), operand(std::move(operand)) {}

Expr::Expr(ExprVariant node) : node(std::move(node)) {}

std::string FunDecl::s_expr(int cur, int ind) {
  std::string res =
      fmt::format("{:{}}(Fun[{}]\n", "", cur, name_params.to_string());
  if (!body->namesp->names.empty()) {
    res += body->namesp->to_string(cur + ind);
  }
  for (auto &var : params) {
    res += fmt::format(
        "{:{}}{} {}\n", "", cur + ind, var->name.str,
        var->type_specifier.value().to_string()
    );
  }
  for (Stmt &stmt : body->stmts) {
    res += stmt.s_expr(cur + ind, ind);
  }
  return res + fmt::format("{:{}})\n", "", cur);
}

std::string Block::to_string(int cur, int ind) {
  std::string res = fmt::format("{:{}}(Block\n", "", cur);
  res += namesp->to_string(cur + ind);
  for (Stmt &stmt : stmts) {
    res += stmt.s_expr(cur + ind, ind);
  }
  return res += fmt::format("{:{}})\n", "", cur);
}

// clang-format off
std::string Declaration::s_expr(int cur, int ind) {
  return std::visit(overload{
    [&](unique_ptr<EnumDecl> &decl) {
      std::string res =
          fmt::format("{:{}}(Enum {}\n", "", cur, decl->name_params.to_string());
      if (!decl->namesp->names.empty()) {
        res += decl->namesp->to_string(cur + ind);
      }
      for (auto &variant : decl->variants) {
        res += fmt::format("{:{}}{} {}\n", "", cur + ind,
            variant.first.str, variant.second.to_string());
      }
      for (auto &fun : decl->methods) {
        res += fmt::format("{:{}}({})\n", "", cur + ind,
            fun->s_expr(cur + ind, ind));
      }
      return res + fmt::format("{:{}})\n", "", cur);
    },
    [&](unique_ptr<FunDecl> &decl) { return decl->s_expr(cur, ind); },
    [&](unique_ptr<StructDecl> &decl) {
      std::string res = fmt::format("{:{}}(Struct "
                                    "{}\n",
          "", cur, decl->name_params.to_string());
      res += decl->namesp->to_string(cur + ind);
      for (auto &field : decl->fields) {
        res += fmt::format(
            "{:{}}{} {}\n", "", cur + ind, field.first.str, field.second.to_string());
      }
      for (auto &fun : decl->methods) {
        res += fun->s_expr(cur + ind, ind);
      }
      return res + fmt::format("{:{}})\n", "", cur);
    },
    [&](unique_ptr<VarDecl> &decl) {
      auto res = fmt::format("{:{}}(Var[{}]", "", cur, decl->name.str);
      if (decl->type_specifier.has_value()) {
        res.append(fmt::format(" {}", decl->type_specifier.value().to_string()));
      }
      if (decl->initializer.has_value()) {
        res.push_back('\n');
        res.append(decl->initializer.value().s_expr(cur + ind, ind));
        res.append(fmt::format("{:{}}", "", cur));
      }
      res += ")\n";
      return res;
    }},
  decl);
}

std::string Stmt::s_expr(int cur, int ind) {
  return std::visit(overload{
    [&](std::monostate) { return fmt::format(";"); },
    [&](unique_ptr<Assign> &stmt) {
      return fmt::format("{:{}}(Assign[{}]\n{}{}{:{}})\n", "", cur,
          to_string(stmt->op), stmt->lhs.s_expr(cur + ind, ind),
          stmt->rhs.s_expr(cur + ind, ind), "", cur);
    },
    [&](unique_ptr<Break> &) { return fmt::format("{:{}}Break\n", cur, ind); },
    [&](unique_ptr<Continue> &) { return fmt::format("{:{}}Continue\n", cur, ind); },
    [&](Declaration &stmt) { return stmt.s_expr(cur, ind); },
    [&](unique_ptr<Expression> &stmt) { return stmt->expr.s_expr(cur, ind); },
    [&](unique_ptr<For> &) { return fmt::format(""); },
    [&](unique_ptr<Return> &stmt) {
      if (stmt->value.has_value()) {
        return fmt::format("{:{}}(Return\n{}{:{}})\n", "", cur,
            stmt->value.value().s_expr(cur+ind, ind), "", cur);
      }
      return fmt::format("{:{}}(Return)\n", "", cur);
    },
    [&](unique_ptr<While> &) { return fmt::format(""); },
    [&](unique_ptr<Print> &stmt) {
      std::string res{};
      res += fmt::format("{:{}}(Print\n", "", cur);
      for (auto &expr : stmt->args) {
        res += expr.s_expr(cur + ind, ind);
      }
      return res + fmt::format("{:{}})\n", "", cur);
    },
  },
  node);
}

std::string to_string(LiteralVariant v) {
  return std::visit(overload{
    [](int32_t l) { return fmt::format("{}", l); },
    [](int64_t l) { return fmt::format("{}", l); },
    [](float l) { return fmt::format("{}", l); },
    [](double l) { return fmt::format("{}", l); },
    [](bool l) { return fmt::format("{}", l); },
    [](char l) { return fmt::format("{}", l); },
    [](std::string l) { return l; }},
  v);
}

std::string Expr::s_expr(int cur, int ind) {
  return std::visit(overload{
    [&](unique_ptr<Binary> &expr) {
      return fmt::format("{:{}}(Binary[{}]\n{}{}{:{}})\n", "", cur,
          to_string(expr->op), expr->left.s_expr(cur + ind, ind),
          expr->right.s_expr(cur + ind, ind), "", cur);
    },
    [&](unique_ptr<Block> &expr) {
      return expr->to_string(cur, ind);
    },
    [&](unique_ptr<DotRef> &expr) {
      return fmt::format("{:{}}(DotRef\n{}{:{}}({})\n{:{}})\n", "", cur,
          expr->lvalue.s_expr(cur + ind, ind), "", cur + ind,
          expr->name.str, "", cur);
    },
    [&](unique_ptr<FunCall> &expr) {
      std::string res = fmt::format("{:{}}(FunCall\n{}", "", cur,
          expr->callee.s_expr(cur + ind, ind));
      for (Expr &e : expr->args) {
        res += fmt::format("{}", e.s_expr(cur + ind, ind));
      }
      return res + fmt::format("{:{}})\n", "", cur);
    },
    [&](unique_ptr<If> &expr) {
      auto res = fmt::format("{:{}}(If\n", "", cur);
      for (auto &branch : expr->branches) {
        res += fmt::format("{:{}}(Branch\n", "", cur);
        res += branch->condition.s_expr(cur+ind, ind);
        res += branch->block->to_string(cur+ind, ind);
      }
      return res + fmt::format("{:{}})\n", "", cur);
    },
    [&](unique_ptr<Index> &expr) {
      auto res = fmt::format("{:{}}(Index\n{}", "", cur,
          expr->callee.s_expr(cur + ind, ind));
      for (Expr &e : expr->args) {
        res += fmt::format("{}", e.s_expr(cur + ind, ind));
      }
      return res + fmt::format("{:{}})\n", "", cur);
    },
    [&](unique_ptr<Literal> &expr) {
      return fmt::format(
          "{:{}}(Literal[{}])\n", "", cur, to_string(expr->val));
    },
    [&](unique_ptr<Unary> &expr) {
      auto res = fmt::format("{:{}}(Unary[{}]\n", "", cur, to_string(expr->op));
      res += expr->operand.s_expr(cur+ind, ind);
      return res + fmt::format("{:{}})\n", "", cur);
    },
    [&](unique_ptr<NamedValue> &expr) {
      return fmt::format(
          "{:{}}(NamedValue[{}])\n", "", cur, expr->name.str);
    },
  },
  node);
}
// clang-format on

AST::AST(vector<Declaration> decls, unique_ptr<Namespace> globals)
    : decls(std::move(decls)), globals(std::move(globals)) {}

std::string AST::to_string() {
  std::string res = "AST\n";
  res += globals->to_string(0);
  for (Declaration &decl : decls) {
    res += decl.s_expr(0, 2) + "\n";
  }
  return res;
}

} // namespace cinnabar
