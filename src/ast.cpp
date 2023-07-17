#include "ast.hpp"

#include "fmt/core.h"

namespace cinnabar {

using std::pair;
using std::unique_ptr;
using std::vector;

using enum Lexeme;

// statements
Assign::Assign(AssignOp op, unique_ptr<Expr> lhs, unique_ptr<Expr> rhs): op(op), lhs(std::move(lhs)), rhs(std::move(rhs)) {}

Declaration::Declaration(DeclVariant decl) : decl(std::move(decl)) {}

// declaration statements
EnumDecl::EnumDecl(Token name, vector<pair<Token, TypeName>> variants,
                   vector<FunDecl> methods, Namespace namesp)
    : name(name), variants(variants), methods(std::move(methods)), namesp(namesp) {}

FunDecl::FunDecl(Token name, vector<pair<Token, TypeName>> params,
                 Block body)
    : name(name), params(params), body(std::move(body)) {}

StructDecl::StructDecl(Token name, vector<pair<Token, TypeName>> fields,
                       vector<FunDecl> methods, Namespace namesp)
    : name(name), fields(fields), methods(std::move(methods)), namesp(namesp) {}

VarDecl::VarDecl(Token name, std::optional<TypeName> type,
                 unique_ptr<Expr> initializer)
    : name(name), type(type), initializer(std::move(initializer)) {}


// expressions
Expression::Expression(Expr expr) : expr(std::move(expr)) {}

Binary::Binary(BinaryOp op, std::unique_ptr<Expr> left,
               std::unique_ptr<Expr> right)
    : op(op), left(std::move(left)), right(std::move(right)) {}

Block::Block(vector<Stmt> stmts, Namespace namesp): stmts(std::move(stmts)), namesp(namesp) {}

DotRef::DotRef(unique_ptr<Expr> lvalue, Token name)
    : lvalue(std::move(lvalue)), name(name) {}

FunCall::FunCall(unique_ptr<Expr> callee, vector<Expr> args)
    : callee(std::move(callee)), args(std::move(args)) {}

If::If(vector<Branch> branches): branches(std::move(branches)) {}

If::Branch::Branch(unique_ptr<Expr> condition, Block block): condition(std::move(condition)), block(std::move(block)) {}

Literal::Literal(LiteralVariant val) : val(val) {}

Variable::Variable(Token name): name(name) {}

Unary::Unary(UnaryOp op, std::unique_ptr<Expr> rhs): op(op), rhs(std::move(rhs)) {}

Expr::Expr(ExprVariant node) : node(std::move(node)) {}

std::string FunDecl::s_expr(int cur, int ind) {
  std::string res = fmt::format("{:{}}(Fun[{}]\n", "", cur, name.str);
  for (auto &p : body.namesp.names) {
    res += fmt::format("{:{}}", "", cur + ind);
    res += fmt::format("({}, {}, {}), ", p.first, p.second.first, p.second.second);
  }
  res += "\n";
  for (auto &p : params) {
    res +=
        fmt::format("{:{}}{} {}\n", "", cur + ind, p.first.str, p.second.name);
  }
  for (Stmt &stmt : body.stmts) {
    res += stmt.s_expr(cur + ind, ind);
  }
  return res + fmt::format("{:{}})\n", "", cur);
}

template <class... Ts> struct overload : Ts... { using Ts::operator()...; };
template <class... Ts> overload(Ts...) -> overload<Ts...>; // helloooo clang???
std::string Declaration::s_expr(int cur, int ind) {
  return std::visit(overload{
    [&](EnumDecl &decl) {
      std::string res = fmt::format("{:{}}(Enum {}\n", "", cur, decl.name.str);
      for (auto &p : decl.namesp.names) {
        res += fmt::format("{:{}}", "", cur + ind);
        res += fmt::format("({}, {}, {}), ", p.first, p.second.first, p.second.second);
      }
      res += "\n";
      for (auto &p : decl.variants) {
       res += fmt::format("{:{}}{} {}\n", "", cur + ind, p.first.str, p.second.name);
      }
      for (auto &fun : decl.methods) {
       res += fmt::format("{:{}}({})\n", "", cur + ind, fun.s_expr(cur + ind, ind));
      }
      return res + fmt::format("{:{}})\n", "", cur);
      },
      [&](FunDecl &decl) { return decl.s_expr(cur, ind); },
      [&](StructDecl &decl) {
      std::string res =
         fmt::format("{:{}}(Struct {}\n", "", cur, decl.name.str);
        if (!decl.namesp.names.empty()) {
          res += fmt::format("{:{}}", "", cur + ind);
          for (auto &p : decl.namesp.names) {
            res += fmt::format("{}[{}, {}], ", p.first, p.second.first, p.second.second);
          }
          res.pop_back();
          res.back() = '\n';
        }
      for (auto &p : decl.fields) {
       res += fmt::format("{:{}}{} {}\n", "", cur + ind,
                          p.first.str, p.second.name);
      }
      for (auto &fun : decl.methods) {
        res += fun.s_expr(cur + ind, ind);
      }
      return res + fmt::format("{:{}})\n", "", cur);
      },
      [&](VarDecl &decl) {
      return fmt::format(
         "{:{}}(Var[{}] {}\n{}{:{}})\n", "", cur, decl.name.str,
         decl.type.has_value() ? decl.type.value().name : "",
         decl.initializer->s_expr(cur + ind, ind), "", cur);
      }},
    decl);
}

std::string Stmt::s_expr(int cur, int ind) {
  return std::visit(
      overload{
          [&](std::monostate) { return fmt::format(";"); },
          [&](Assign &expr) { return fmt::format("{:{}}(Assign[{}]\n{}{}{:{}})\n", "", cur,
              to_string(expr.op), expr.lhs->s_expr(cur+ind, ind), expr.rhs->s_expr(cur+ind, ind), "", cur);
          },
          [&](Break &) { return fmt::format("{:{}}Break", cur, ind); },
          [&](Continue &) { return fmt::format("{:{}}Continue", cur, ind); },
          [&](Declaration &stmt) { return stmt.s_expr(cur, ind); },
          [&](Expression &stmt) { return stmt.expr.s_expr(cur, ind); },
          [&](For &) { return fmt::format(""); },
          [&](Return &) { return fmt::format("{:{}}Return", cur, ind); },
          [&](While &) { return fmt::format(""); },
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
  return std::visit(
      overload{
          [&](Binary &expr) {
            return fmt::format("{:{}}(Binary[{}]\n{}{}{:{}})\n", "", cur,
                               to_string(expr.op),
                               expr.left->s_expr(cur + ind, ind),
                               expr.right->s_expr(cur + ind, ind), "", cur);
          },
          [&](Block &expr) {
            std::string res = fmt::format("{:{}}(Block\n", "", cur);
            for (auto &p : expr.namesp.names) {
              res += fmt::format("{:{}}", "", cur + ind);
              res += fmt::format("({}, {}, {}), ", p.first, p.second.first, p.second.second);
            }
            res += "\n";
            for (Stmt &stmt : expr.stmts) {
              res += stmt.s_expr(cur + ind, ind);
            }
            return res += fmt::format("{:{}})\n", "", cur);
            },
          [&](DotRef &expr) {
            return fmt::format("{:{}}(DotRef\n{}{:{}}({})\n{:{}})\n", "", cur,
                               expr.lvalue->s_expr(cur + ind, ind), "", cur+ind,
                               expr.name.str, "", cur);
          },
          [&](FunCall &expr) {
            std::string res =
                fmt::format("{:{}}(FunCall\n{}", "", cur,
                            expr.callee->s_expr(cur + ind, ind));
            for (Expr &e : expr.args) {
              res += fmt::format("{}", e.s_expr(cur + ind, ind));
            }
            return res + fmt::format("{:{}})\n", "", cur);
          },
          [&](If &) { return fmt::format("{:{}}(If)", "", cur); },
          [&](Literal &expr) {
            return fmt::format("{:{}}(Literal[{}])\n", "", cur,
                               to_string(expr.val));
          },
          [&](Unary &) { return fmt::format("{:{}}(Unary)\n", "", cur); },
          [&](Variable &expr) { return fmt::format("{:{}}(Variable[{}])\n", "", cur, expr.name.str); },
      },
      node);
}

AST::AST(std::vector<Declaration> decls, Namespace globals) : decls(std::move(decls)), globals(globals) {}

std::string AST::to_string() {
  std::string res = "AST\n";
  for (auto &p : globals.names) {
    res += fmt::format("({}, {}, {}), ", p.first, p.second.first, p.second.second);
  }
  res += "\n";
  for (Declaration &decl : decls) {
    res += decl.s_expr(0, 2) + "\n";
  }
  return res;
}

} // namespace cinnabar
