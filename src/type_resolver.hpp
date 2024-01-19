#pragma once

#include "ast.hpp"
#include "tast.hpp"

#include <unordered_set>

namespace cinnabar {

struct TypeResolver {
  class PushNamespace {
    TypeResolver *tr;
    Namespace *prev_namesp;
    TNamespace *prev_tnamesp;

  public:
    inline PushNamespace(
        TypeResolver *tr, Namespace *namesp, TNamespace *tnamesp
    )
        : tr(tr), prev_namesp(tr->cur_ast_namesp),
          prev_tnamesp(tr->cur_tnamesp) {
      tr->cur_ast_namesp = namesp;
      tr->cur_tnamesp = tnamesp;
    }
    PushNamespace(const PushNamespace &) = delete;
    PushNamespace(const PushNamespace &&) = delete;
    PushNamespace operator=(const PushNamespace &) = delete;
    PushNamespace operator=(const PushNamespace &&) = delete;
    inline ~PushNamespace() {
      tr->cur_ast_namesp = prev_namesp;
      tr->cur_tnamesp = prev_tnamesp;
    }
  };
  class PushFun {
    TypeResolver *tr;
    FunctionInst *prev_fun;
    TNamespace *prev_fun_tnamesp;

  public:
    inline PushFun(
        TypeResolver *tr, FunctionInst *new_fun, TNamespace *new_fun_tnamesp
    )
        : tr(tr), prev_fun(tr->cur_fun), prev_fun_tnamesp(tr->cur_fun_tnamesp) {
      tr->cur_fun = new_fun;
      tr->cur_fun_tnamesp = new_fun_tnamesp;
    }
    inline ~PushFun() {
      tr->cur_fun = prev_fun;
      tr->cur_fun_tnamesp = prev_fun_tnamesp;
    }
  };

  AST &ast;

  std::vector<Item> items;
  std::vector<TypeRep> types;

  // avoid cycles, index of items
  std::unordered_set<size_t> currently_creating;
  std::vector<int> type_topo_order;
  std::vector<TNamespace *> namespaces;

  std::unordered_map<std::string_view, TypeId> builtin_types;
  std::unordered_map<std::string, TypeId> function_types;

  Namespace *cur_ast_namesp = nullptr;
  TNamespace *cur_tnamesp = nullptr;

  TNamespace *root_tnamesp = nullptr;

  FunctionInst *cur_fun = nullptr;
  TNamespace *cur_fun_tnamesp = nullptr;

  PushNamespace push_namespace(Namespace *namesp, TNamespace *tnamesp);
  PushFun push_fun(FunctionInst *fun, TNamespace *fun_tnamesp);
  
  std::unique_ptr<TNamespace> make_tnamespace(TNamespace *parent_tnamesp);

  TypeResolver(AST &ast);

  TGenericArg resolve(const GenericArg &arg);
  TGenericInst resolve(const GenericInst &inst);
  std::unique_ptr<TLiteral> resolve(const Literal &literal);

  // retrieve items from namespace hierarchy
  TypeId get_type(TGenericInst concrete);
  FunctionInst *get_function(TGenericInst concrete);
  TVarInst *get_var_local_global(std::string_view name);
  DeclPtr get_decl(const std::string &name);
  TypeId get_function_typeid(const FunctionType &ft);

  // tnamesp is the TNamespace where the type will be declared
  TypeId create_struct_type(
      StructDecl *decl, std::vector<TGenericArg> args,
      TNamespace *parent_tnamesp
  );
  TypeId create_enum_type(
      EnumDecl *decl, std::vector<TGenericArg> args,
      TNamespace *parent_tnamesp
  );
  TypeId create_ref_type(TGenericInst concrete);
  TypeId create_array_type(TGenericInst concrete);
  TypeId create_span_type(TGenericInst concrete);

  FunctionInst *create_function(FunDecl *decl, std::vector<TGenericArg> args, TNamespace *parent_tnamesp);
  
  // must be called with an Index with NamedValue callee
  TGenericInst index_to_tgeneric_inst(Index &index);

  TypeId find_unary_op(UnaryOp op, const TExpr &operand);
  TypeId find_binary_op(BinaryOp op, TypeId lhs_type, TypeId rhs_type);

  // instantiates a concrete typed function from a (potentially generic)
  // untyped function definition
  FunctionInst *
  add_fun(const GenericInst &fun, FunDecl *decl, TNamespace *parent_tnamesp);

  std::optional<TStmt> resolve(Stmt &stmt);
  std::optional<std::unique_ptr<TVarInst>> resolve(Declaration &decl);
  std::unique_ptr<TVarInst> resolve(VarDecl &decl);

  TExpr resolve(Expr &expr);
  std::unique_ptr<TBlock> resolve(Block &block);
  std::unique_ptr<TBlock>
  resolve(Block &block, std::unique_ptr<TNamespace> tnamesp);
  TAST resolve();

  [[nodiscard]] std::string to_string(TypeId id) const;
  [[noreturn]] void error(std::string_view message);
  [[noreturn]] void error(TypeId expected, TypeId found);

  inline bool get_has_error() { return false; }
};

} // namespace cinnabar
