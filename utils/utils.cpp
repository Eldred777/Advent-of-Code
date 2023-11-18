#include <fstream>
#include <iostream>
#include <string>

void handle_file_stream(std::ifstream& is, std::string rel_filepath) {
  is.open(rel_filepath);

  if (!is.is_open()) {
    is.close();
    int input_name_idx = rel_filepath.find('/', 6) + 1;
    is.open(rel_filepath.substr(input_name_idx));
    if (!is.is_open()) {
      std::cerr << "Input file stream failed to open.";
      exit(1);
    }
  }
}
