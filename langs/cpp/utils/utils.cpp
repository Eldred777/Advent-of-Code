#include <fstream>
#include <iostream>
#include <string>

// macro to report results; not a fn so we avoid strange execution behaviour
#define report_results(p1, p2)                                                 \
  std::cout << "Part 1: " << p1 << '\n';                                       \
  std::cout << "Part 2: " << p2 << std::endl;

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

// -------------------------
// String processing functions
// -------------------------

/* Returns index of `n+1`th word after the word at `offset`. Note that it does
this by counting spaces; double spaces or a string starting with a space will
cause issues.  */
size_t skip_n_words(std::string s, size_t n, size_t offset) {
  size_t i = offset;
  if (s.at(i) == ' ') {
    n--;
  }
  size_t j = 0;
  while (j < n) {
    i = s.find(' ', i + 1);
    j++;
  }
  return i + 1;
}
