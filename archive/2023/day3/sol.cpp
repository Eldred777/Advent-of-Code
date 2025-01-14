#include <fstream>
#include <iostream>
#include <string>
#include "../../utils/utils.cpp"

// part 1
bool is_number(char c) {
  return '0' <= c && c <= '9';
}
bool is_symbol(char c) {
  // Exhaustive search of all symbols done via `misc.cl` script
  switch (c) {
    case '@':
    case '$':
    case '%':
    case '-':
    case '#':
    case '&':
    case '/':
    case '+':
    case '*':
    case '=':
      return true;

    default:
      return false;
  }
}

int search_line_for_adjacent_numbers(const std::string& search_line,
                                     int index,
                                     bool ignore_middle) {
  // IDEA: search middle, and if not number, look at left and right.
  int acc = 0;
  if (!ignore_middle && is_number(search_line.at(index))) {
    int left_index = index, right_index = index;
    while (left_index > 0 && is_number(search_line.at(left_index - 1))) {
      left_index--;
    }
    while (right_index < search_line.length() &&
           is_number(search_line.at(right_index))) {
      right_index++;
    }
    acc += std::stoi(search_line.substr(left_index, right_index - left_index));
  } else {
    // search left
    if (index > 0 && is_number(search_line.at(index - 1))) {
      int left_index = index;
      while (left_index > 0 && is_number(search_line.at(left_index - 1))) {
        left_index--;
      }
      acc += std::stoi(search_line.substr(left_index, index - left_index));
    }

    // search right
    if (index < search_line.length() && is_number(search_line.at(index + 1))) {
      int right_index = index;
      while (right_index < search_line.length() &&
             is_number(search_line.at(right_index))) {
        right_index++;
      }
      acc += std::stoi(search_line.substr(index + 1, right_index - index - 1));
    }
  }
  return acc;
}

int search_for_adjacent_numbers(const std::string& prev_line,
                                const std::string& line,
                                const std::string& next_line,
                                int index,
                                bool is_top_line,
                                bool is_bottom_line) {
  int acc = 0;
  acc += search_line_for_adjacent_numbers(line, index, true);
  if (!is_top_line) {
    acc += search_line_for_adjacent_numbers(prev_line, index, false);
  }
  if (!is_bottom_line) {
    acc += search_line_for_adjacent_numbers(next_line, index, false);
  }
  return acc;
}

int search_line_for_symbols(const std::string& prev_line,
                            const std::string& line,
                            const std::string& next_line,
                            bool is_top_line,
                            bool is_bottom_line) {
  int acc = 0;
  for (int i = 0; i < line.length(); ++i) {
    if (is_symbol(line.at(i))) {
      // ignore first arg
      acc += search_for_adjacent_numbers(prev_line, line, next_line, i,
                                         is_top_line, is_bottom_line);
    }
  }
  return acc;
}

template <typename T>
void copy_left(T& x, T& y, T& z) {
  x = y;
  y = z;
}

int part1(std::istream& is) {
  int acc = 0;
  std::string prev_line, line, next_line;

  // top row
  std::getline(is, line);
  std::getline(is, next_line);
  acc += search_line_for_symbols(std::string(""), line, next_line, true, false);
  copy_left(prev_line, line, next_line);

  // middle rows
  while (std::getline(is, next_line)) {
    acc += search_line_for_symbols(prev_line, line, next_line, false, false);
    copy_left(prev_line, line, next_line);
  }

  // bottom row
  acc += search_line_for_symbols(prev_line, line, std::string(""), false, true);

  return acc;
}

// part 2
bool is_gear_candidate(char c) {
  return c == '*';
}

constexpr int GEAR_COUNT = 2;

struct gear_acc {
  int acc = 1;
  int count = 0;

  void update_gear_ratio(int ratio) {
    this->count++;
    this->acc *= ratio;
  }

  void add_gear(const gear_acc& other) {
    this->acc *= other.acc;
    this->count += other.count;
  }
};

gear_acc gear_search_line_for_adjacent_numbers(const std::string& search_line,
                                               int index,
                                               bool ignore_middle) {
  // IDEA: search middle, and if not number, look at left and right.
  gear_acc gear;
  if (!ignore_middle && is_number(search_line.at(index))) {
    int left_index = index, right_index = index;
    while (left_index > 0 && is_number(search_line.at(left_index - 1))) {
      left_index--;
    }
    while (right_index < search_line.length() &&
           is_number(search_line.at(right_index))) {
      right_index++;
    }
    gear.update_gear_ratio(
        std::stoi(search_line.substr(left_index, right_index - left_index)));
  } else {
    // search left
    if (index > 0 && is_number(search_line.at(index - 1))) {
      int left_index = index;
      while (left_index > 0 && is_number(search_line.at(left_index - 1))) {
        left_index--;
      }
      gear.update_gear_ratio(
          std::stoi(search_line.substr(left_index, index - left_index)));
    }

    // search right
    if (index < search_line.length() && is_number(search_line.at(index + 1))) {
      int right_index = index;
      while (right_index < search_line.length() &&
             is_number(search_line.at(right_index))) {
        right_index++;
      }
      gear.update_gear_ratio(
          std::stoi(search_line.substr(index + 1, right_index - index - 1)));
    }
  }
  return gear;
}

gear_acc gear_search_for_adjacent_numbers(const std::string& prev_line,
                                          const std::string& line,
                                          const std::string& next_line,
                                          int index,
                                          bool is_top_line,
                                          bool is_bottom_line) {
  gear_acc gear;
  gear.add_gear(gear_search_line_for_adjacent_numbers(line, index, true));
  if (!is_top_line) {
    gear.add_gear(
        gear_search_line_for_adjacent_numbers(prev_line, index, false));
  }
  if (!is_bottom_line) {
    gear.add_gear(
        gear_search_line_for_adjacent_numbers(next_line, index, false));
  }
  return gear;
}

int search_line_for_gears(const std::string& prev_line,
                          const std::string& line,
                          const std::string& next_line,
                          bool is_top_line,
                          bool is_bottom_line) {
  int acc = 0;
  for (int i = 0; i < line.length(); ++i) {
    if (is_symbol(line.at(i))) {
      // ignore first arg
      auto temp = gear_search_for_adjacent_numbers(
          prev_line, line, next_line, i, is_top_line, is_bottom_line);
      if (temp.count == GEAR_COUNT)
        acc += temp.acc;
    }
  }
  return acc;
}

int part2(std::istream& is) {
  int acc = 0;
  std::string prev_line, line, next_line;

  // top row
  std::getline(is, line);
  std::getline(is, next_line);
  acc += search_line_for_gears(std::string(""), line, next_line, true, false);
  copy_left(prev_line, line, next_line);

  // middle rows
  while (std::getline(is, next_line)) {
    acc += search_line_for_gears(prev_line, line, next_line, false, false);
    copy_left(prev_line, line, next_line);
  }

  // bottom row
  acc += search_line_for_gears(prev_line, line, std::string(""), false, true);

  return acc;
}

int main() {
  std::ifstream infile;
  handle_file_stream(infile, "2023/day3/input");

  std::cout << "Part 1: " << part1(infile) << std::endl;
  reset_file_stream(infile);
  std::cout << "Part 2: " << part2(infile);

  return 0;
}
