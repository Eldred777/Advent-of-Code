#include <fstream>
#include <iostream>
#include <string>

void handle_file_stream(std::ifstream &is, std::string rel_filepath) {
  is.open(rel_filepath);
  if (is.is_open()) {
    return;
  }
  is.close();
  // 6 for "input"
  int input_name_idx = rel_filepath.find('/', 6) + 1;
  is.open(rel_filepath.substr(input_name_idx));
  if (!is.is_open()) {
    std::cerr << "Input file stream failed to open.";
    exit(1);
  }
}

void reset_file_stream(std::ifstream &is) {
  is.clear();
  is.seekg(0, std::ios::beg);
}

void report_results(int p1, int p2) {
  std::cout << "Part 1: " << p1 << '\n';
  std::cout << "Part 2: " << p2 << std::endl;
}
