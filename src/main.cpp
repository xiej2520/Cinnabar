#include "lexer.hpp"
#include "parser.hpp"
#include "typecheck_resolver.hpp"

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

#include <memory>
int main(int argc, char **argv) {
  if (argc != 2) {
    fmt::print("Usage: cinnabar [file]\n");
    exit(1);
  }

  std::string src = read_file(argv[1]);
  cinnabar::Lexer lexer(src);

  lexer.lex();
  fmt::print("{}\n\n", lexer.token_repr());

  cinnabar::Parser parser(src);
  cinnabar::AST ast = parser.parse();
  fmt::print("{}\n\n", ast.to_string());
  
  cinnabar::TypeResolver resolver(ast);
  resolver.resolve();
}
