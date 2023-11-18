#include "Jayson.h"
#include <fstream>
#include <iostream>
#include <sstream>

typedef std::basic_stringstream<char32_t> u32sstream;
typedef std::basic_ifstream<char32_t> u32filesstream;

std::basic_string<char32_t> read_file(char *file_path) {
  u32filesstream input_file(file_path);
  u32sstream buffer;
  buffer << input_file.rdbuf();
  return buffer.str();
}

int main(int argc, char **argv) {

  // read file
  auto input = read_file(argv[1]);

  // init parser
  Jayson::Parser parser{input};

  try {
    // parse file
    auto result = parser.parse();

    // print json node
    std::cout << result;

    return 0;
  } catch (const std::invalid_argument &error) {
    std::cout << "Error: " << error.what();
    return 1;
  }
}
