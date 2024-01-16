#include "lexer.hpp"
#include "parser.hpp"
#include "type_resolver.hpp"
#include "codegen.hpp"

#include "argparse/argparse.hpp"

#include <cstdlib>
#include <fstream>
#include <sstream>
#include <string>

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

void write_file(const std::string &path, std::string_view output) {
  std::ofstream f(path);
  if (!f.is_open()) {
    fmt::print(stderr, "Could not open file \"{}\" for writing.\n", path);
    exit(74);
  }
  f << output;
}

} // namespace

int main(int argc, char **argv) {
  //if (argc != 2) {
  //  fmt::print("Usage: cinnabar [file]\n");
  //  exit(1);
  //}
  
  struct Args : public argparse::Args {
    std::string &src = arg("Source file");
    std::string &c_out = kwarg("c", "File name for the .c output.").set_default("a.c");
    std::string &out = kwarg("o", "File name for the output.").set_default("a.out");
    bool &format     = flag("f,format", "Use clang-format on the output");
  };
  
    auto args = argparse::parse<Args>(argc, argv, true);
    
    auto src = read_file(args.src);
    cinnabar::Lexer lexer(src);

    lexer.lex();
    //fmt::print(stderr, "{}\n\n", lexer.token_repr());

    cinnabar::Parser parser(src);
    auto ast = parser.parse();

    fmt::print(stderr, "{}\n--------\n", ast.to_string());
    
    cinnabar::TypeResolver resolver(ast);
    auto tast = resolver.resolve();

    
    auto res = cinnabar::generate(tast, cinnabar::CodegenOutput::C);
    
    fmt::print("{}", res);
    
    write_file(args.c_out, res);
    if (args.format) {
      std::system(fmt::format("clang-format -i {}", args.c_out).c_str());
    }
    std::system(fmt::format("cc {} -o {}", args.c_out, args.out).c_str());
    fmt::print(stderr, "\n--------\n");
    
    enum Status : int {
      Ok,
      LexError,
      ParseError,
      TypeError,
      CodegenError,
    };
    if (lexer.get_has_error()) return Status::LexError;
    if (parser.get_has_error()) return Status::ParseError;
    if (resolver.get_has_error()) return Status::TypeError;
    return Status::Ok;
}
