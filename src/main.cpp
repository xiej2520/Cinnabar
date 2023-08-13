#include "lexer.hpp"
#include "parser.hpp"
#include "type_resolver.hpp"
#include "codegen.hpp"

#include "fmt/core.h"
#include <fstream>
#include <sstream>

namespace {

std::string read_file(const std::string &path) {
  std::ifstream f(path);
  if (!f.is_open()) {
    fmt::print(stderr, "Could not open file \"{}\".\n", path);
    exit(74);
  }
  std::stringstream contents;
  contents << f.rdbuf();
  return contents.str();
}

} // namespace

int main(int argc, char **argv) {
  if (argc != 2) {
    fmt::print("Usage: cinnabar [file]\n");
    exit(1);
  }

  std::string src = read_file(argv[1]);
  cinnabar::Lexer lexer(src);

  lexer.lex();
  //fmt::print(stderr, "{}\n\n", lexer.token_repr());

  cinnabar::Parser parser(src);
  cinnabar::AST ast = parser.parse();
  
  cinnabar::TypeResolver resolver(ast);
  resolver.resolve();

  fmt::print(stderr, "{}\n\n--------\n\n", ast.to_string());
  
  std::string res = cinnabar::generate(ast, cinnabar::CodegenOutput::C);
  fmt::print("{}", res);
}
