#include "ast.hpp"

#include "fmt/core.h"

namespace cinnabar {

using std::pair;
using std::unique_ptr;
using std::vector;

using enum Lexeme;


std::string GenType::to_string() {
  auto res = name;
  if (!params.empty()) {
    res.push_back('[');
    for (auto &child : params) {
      res.append(child.to_string()).push_back(',');
    }
    res.back() = ']';
  }
  return res;
}

// statements
Assign::Assign(AssignOp op, Expr lhs, Expr rhs)
    : op(op), lhs(std::move(lhs)), rhs(std::move(rhs)) {}

Declaration::Declaration(DeclVariant decl) : decl(std::move(decl)) {}

// declaration statements
EnumDecl::EnumDecl(Token name, vector<pair<Token, GenType>> variants,
    vector<unique_ptr<FunDecl>> methods, Namespace namesp)
    : name(name), variants(variants), methods(std::move(methods)),
      namesp(namesp) {}

FunDecl::FunDecl(
    Token name, vector<unique_ptr<VarDecl>> params, unique_ptr<Block> body)
    : name(name), params(std::move(params)), body(std::move(body)) {}

StructDecl::StructDecl(Token name, vector<pair<Token, GenType>> fields,
    vector<unique_ptr<FunDecl>> methods, Namespace namesp)
    : name(name), fields(fields), methods(std::move(methods)), namesp(namesp) {}

VarDecl::VarDecl(Token name, std::optional<GenType> type_specifier, Expr initializer)
    : name(name), type_specifier(type_specifier),
      initializer(std::move(initializer)) {}

// expressions
Expression::Expression(Expr expr) : expr(std::move(expr)) {}

Binary::Binary(BinaryOp op, Expr left, Expr right)
    : op(op), left(std::move(left)), right(std::move(right)) {}

Block::Block(vector<Stmt> stmts, Namespace namesp)
    : stmts(std::move(stmts)), namesp(namesp) {}

DotRef::DotRef(Expr lvalue, Token name)
    : lvalue(std::move(lvalue)), name(name) {}

FunCall::FunCall(Expr callee, vector<Expr> args)
    : callee(std::move(callee)), args(std::move(args)) {}

If::If(vector<std::unique_ptr<Branch>> branches)
    : branches(std::move(branches)) {}

If::Branch::Branch(Expr condition, unique_ptr<Block> block)
    : condition(std::move(condition)), block(std::move(block)) {}

Literal::Literal(LiteralVariant val) : val(val) {}

Variable::Variable(Token name) : name(name) {}

Unary::Unary(UnaryOp op, Expr operand) : op(op), operand(std::move(operand)) {}

Expr::Expr(ExprVariant node) : node(std::move(node)) {}

std::string FunDecl::s_expr(int cur, int ind) {
  std::string res = fmt::format("{:{}}(Fun[{}]\n", "", cur, name.str);
  if (!body->namesp.names.empty()) {
    res += fmt::format("{:{}}Names: ", "", cur + ind);
    for (auto &name : body->namesp.names) {
      res += fmt::format("{}, ", name);
    }
    res.pop_back();
    res.back() = '\n';
  }
  for (auto &var : params) {
    res += fmt::format(
        "{:{}}{} {}\n", "", cur + ind, var->name.str, var->type_specifier.value().to_string());
  }
  for (Stmt &stmt : body->stmts) {
    res += stmt.s_expr(cur + ind, ind);
  }
  return res + fmt::format("{:{}})\n", "", cur);
}

// clang-format off
std::string Declaration::s_expr(int cur, int ind) {
  return std::visit(overload{
    [&](unique_ptr<EnumDecl> &decl) {
      std::string res =
          fmt::format("{:{}}(Enum {}\n", "", cur, decl->name.str);
      if (!decl->namesp.names.empty()) {
        res += fmt::format("{:{}}Names: ", "", cur + ind);
        for (auto &name : decl->namesp.names) {
          res += fmt::format("{}, ", name);
        }
        res.pop_back();
        res.back() = '\n';
      }
      for (auto &p : decl->variants) {
        res += fmt::format("{:{}}{} {}\n", "", cur + ind,
            p.first.str, p.second.name);
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
          "", cur, decl->name.str);
      if (!decl->namesp.names.empty()) {
        res += fmt::format("{:{}}", "", cur + ind);
        for (auto &name : decl->namesp.names) {
          res += fmt::format("({}), ", name);
        }
        res.pop_back();
        res.back() = '\n';
      }
      for (auto &p : decl->fields) {
        res += fmt::format(
            "{:{}}{} {}\n", "", cur + ind, p.first.str, p.second.name);
      }
      for (auto &fun : decl->methods) {
        res += fun->s_expr(cur + ind, ind);
      }
      return res + fmt::format("{:{}})\n", "", cur);
    },
    [&](unique_ptr<VarDecl> &decl) {
      return fmt::format("{:{}}(Var[{}] "
                         "{}\n{}{:{}})\n",
          "", cur, decl->name.str,
          decl->type_specifier.has_value()
              ? decl->type_specifier.value().name
              : "",
          decl->initializer.s_expr(cur + ind, ind), "", cur);
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
    [&](unique_ptr<Break> &) { return fmt::format("{:{}}Break", cur, ind); },
    [&](unique_ptr<Continue> &) { return fmt::format("{:{}}Continue", cur, ind); },
    [&](Declaration &stmt) { return stmt.s_expr(cur, ind); },
    [&](unique_ptr<Expression> &stmt) { return stmt->expr.s_expr(cur, ind); },
    [&](unique_ptr<For> &) { return fmt::format(""); },
    [&](unique_ptr<Return> &) { return fmt::format("{:{}}Return", cur, ind); },
    [&](unique_ptr<While> &) { return fmt::format(""); },
  },
  node);
}

std::string to_string(LiteralVariant v) {
  return std::visit(overload{
    [](i32 l) { return fmt::format("{}", l); },
    [](i64 l) { return fmt::format("{}", l); },
    [](f32 l) { return fmt::format("{}", l); },
    [](f64 l) { return fmt::format("{}", l); },
    [](bool l) { return fmt::format("{}", l); },
    [](char l) { return fmt::format("{}", l); },
    [](std::string l) { return l; }},
  v);
}

std::string Expr::s_expr(int cur, int ind) {
  return std::visit(overload{
    [&](std::monostate) {
      return fmt::format("{:{}}[monostate]", "", cur);
    },
    [&](unique_ptr<Binary> &expr) {
      return fmt::format("{:{}}(Binary[{}]\n{}{}{:{}})\n", "", cur,
          to_string(expr->op), expr->left.s_expr(cur + ind, ind),
          expr->right.s_expr(cur + ind, ind), "", cur);
    },
    [&](unique_ptr<Block> &expr) {
      std::string res = fmt::format("{:{}}(Block\n", "", cur);
      if (!expr->namesp.names.empty()) {
        res += fmt::format("{:{}}Names: ", "", cur + ind);
        for (auto &name : expr->namesp.names) {
          res += fmt::format("{}, ", name);
        }
        res.pop_back();
        res.back() = '\n';
      }
      for (Stmt &stmt : expr->stmts) {
        res += stmt.s_expr(cur + ind, ind);
      }
      return res += fmt::format("{:{}})\n", "", cur);
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
    [&](unique_ptr<If> &) { return fmt::format("{:{}}(If)", "", cur); },
    [&](unique_ptr<Literal> &expr) {
      return fmt::format(
          "{:{}}(Literal[{}])\n", "", cur, to_string(expr->val));
    },
    [&](unique_ptr<Unary> &) {
      return fmt::format("{:{}}(Unary)\n", "", cur);
    },
    [&](unique_ptr<Variable> &expr) {
      return fmt::format(
          "{:{}}(Variable[{}])\n", "", cur, expr->name.str);
    },
  },
  node);
}
// clang-format on

AST::AST(std::vector<Declaration> decls, Namespace globals)
    : decls(std::move(decls)), globals(globals) {}

std::string AST::to_string() {
  std::string res = "AST\n";
  for (auto &name : globals.names) {
    res += fmt::format("({}), ", name);
  }
  res += "\n";
  for (Declaration &decl : decls) {
    res += decl.s_expr(0, 2) + "\n";
  }
  return res;
}

} // namespace cinnabar
