#include <fstream>
#include <iostream>
#include <string>

/* This allows me to run files either from the repo directory, or from the
directory containing the source code. */
void handle_file_stream(std::ifstream &is, std::string rel_filepath) {
  is.open(rel_filepath);
  if (is.is_open()) {
    return;
  }
  is.close();
  // 6 for "input"
  int input_name_idx = rel_filepath.find('/', 6) + 1;
  is.open(rel_filepath.substr(input_name_idx, std::string::npos));
  if (!is.is_open()) {
    std::cerr << "Input file stream failed to open with filepath: "
              << rel_filepath;
    exit(1);
  }
}

/* Allows me to reparse the input for part 2 when needed.
Probably shouldn't need to use it but here anyway! */
void reset_file_stream(std::ifstream &is) {
  is.clear();
  is.seekg(0, std::ios::beg);
}

// Convenience fn.
void report_results(int p1, int p2) {
  std::cout << "Part 1: " << p1 << '\n';
  std::cout << "Part 2: " << p2 << std::endl;
}
