#include "../../utils/utils.cpp"
#include <array>
#include <fstream>
#include <iostream>
#include <string>

// #define TEST
#ifdef TEST
const std::string FILE_NAME = "2015/day18/test-input";
constexpr size_t NUM_STEPS = 4;
constexpr size_t GRID_SIZE = 6;
#else
const std::string FILE_NAME = "2015/day18/input";
constexpr size_t NUM_STEPS = 100;
constexpr size_t GRID_SIZE = 100;
#endif // TEST

int count_neighbours_on_row(const std::array<bool, GRID_SIZE> &row, size_t j) {
  int acc = 0;
  acc += row.at(j);
  if (j == 0) {
    acc += row.at(j + 1);
  } else if (j == GRID_SIZE - 1) {
    acc += row.at(j - 1);

  } else {
    acc += row.at(j + 1);
    acc += row.at(j - 1);
  }
  return acc;
}

// same as `count_neighbours_on_row` except does not add the middle value
int count_neighbours_on_middle_row(const std::array<bool, GRID_SIZE> &row,
                                   size_t j) {
  int acc = 0;
  if (j == 0) {
    acc += row.at(j + 1);
  } else if (j == GRID_SIZE - 1) {
    acc += row.at(j - 1);

  } else {
    acc += row.at(j + 1);
    acc += row.at(j - 1);
  }
  return acc;
}

int count_neighbours(
    const std::array<std::array<bool, GRID_SIZE>, GRID_SIZE> &grid, size_t i,
    size_t j) {
  int acc = 0;
  acc += count_neighbours_on_middle_row(grid.at(i), j);
  if (i == 0) {
    acc += count_neighbours_on_row(grid.at(i + 1), j);
  } else if (i == GRID_SIZE - 1) {
    acc += count_neighbours_on_row(grid.at(i - 1), j);
  } else {
    acc += count_neighbours_on_row(grid.at(i - 1), j);
    acc += count_neighbours_on_row(grid.at(i + 1), j);
  }
  return acc;
}

int part1(std::ifstream &is) {
  std::string line;
  std::array<std::array<bool, GRID_SIZE>, GRID_SIZE> light_grid;
  std::array<std::array<int, GRID_SIZE>, GRID_SIZE> neighbours;
  // get initial state
  int line_count = 0;
  while (std::getline(is, line)) {
    for (int i = 0; i < line.size(); ++i) {
      switch (line.at(i)) {
      case '#':
        light_grid.at(line_count).at(i) = true;
        break;
      case '.':
        light_grid.at(line_count).at(i) = false;
        break;
      default:
        std::cerr << "Bad index given";
        exit(1);
      }
    }
    line_count++;
  }
  // simulate
  for (size_t t = 0; t < NUM_STEPS; ++t) {
    // process current time
    for (size_t i = 0; i < GRID_SIZE; ++i) {   // iter over rows
      for (size_t j = 0; j < GRID_SIZE; ++j) { // iter over columns
        neighbours.at(i).at(j) = count_neighbours(light_grid, i, j);
      }
    }
    // evolve to next time
    for (size_t i = 0; i < GRID_SIZE; ++i) {
      for (size_t j = 0; j < GRID_SIZE; ++j) {
        bool &light_on = light_grid.at(i).at(j);
        const int neighbour_count = neighbours.at(i).at(j);
        if (light_on && !(neighbour_count == 2 || neighbour_count == 3)) {
          light_on = false;
        } else if (!light_on && neighbour_count == 3) {
          light_on = true;
        }
      }
    }
  }
  // count how many lights are on
  int acc = 0;
  for (size_t i = 0; i < GRID_SIZE; ++i) {
    for (size_t j = 0; j < GRID_SIZE; ++j) {
      bool &light_on = light_grid.at(i).at(j);
      if (light_on) {
        acc++;
      }
    }
  }
  return acc;
}

inline bool is_corner(size_t i, size_t j) {
  return ((i == 0 || i == GRID_SIZE - 1) && (j == 0 || j == GRID_SIZE - 1));
}

int part2(std::ifstream &is) {
  reset_file_stream(is);
  std::string line;
  std::array<std::array<bool, GRID_SIZE>, GRID_SIZE> light_grid;
  std::array<std::array<int, GRID_SIZE>, GRID_SIZE> neighbours;
  // get initial state
  int line_count = 0;
  while (std::getline(is, line)) {
    for (int i = 0; i < line.size(); ++i) {
      switch (line.at(i)) {
      case '#':
        light_grid.at(line_count).at(i) = true;
        break;
      case '.':
        light_grid.at(line_count).at(i) = false;
        break;
      default:
        std::cerr << "Bad index given";
        exit(1);
      }
    }
    line_count++;
  }
  // Set corners
  light_grid.at(0).at(0) = true;
  light_grid.at(0).at(GRID_SIZE - 1) = true;
  light_grid.at(GRID_SIZE - 1).at(0) = true;
  light_grid.at(GRID_SIZE - 1).at(GRID_SIZE - 1) = true;
  // simulate
  for (size_t t = 0; t < NUM_STEPS; ++t) {
    // process current time
    for (size_t i = 0; i < GRID_SIZE; ++i) {   // iter over rows
      for (size_t j = 0; j < GRID_SIZE; ++j) { // iter over columns
        neighbours.at(i).at(j) = count_neighbours(light_grid, i, j);
      }
    }
    // evolve to next time
    for (size_t i = 0; i < GRID_SIZE; ++i) {
      for (size_t j = 0; j < GRID_SIZE; ++j) {
        if (is_corner(i, j))
          continue;

        bool &light_on = light_grid.at(i).at(j);
        const int neighbour_count = neighbours.at(i).at(j);
        if (light_on && !(neighbour_count == 2 || neighbour_count == 3)) {
          light_on = false;
        } else if (!light_on && neighbour_count == 3) {
          light_on = true;
        }
      }
    }
  }
  // count how many lights are on
  int acc = 0;
  for (size_t i = 0; i < GRID_SIZE; ++i) {
    for (size_t j = 0; j < GRID_SIZE; ++j) {
      bool &light_on = light_grid.at(i).at(j);
      if (light_on) {
        acc++;
      }
    }
  }
  return acc;
}

int main() {
  //
  std::ifstream is;
  handle_file_stream(is, FILE_NAME);
  report_results(part1(is), part2(is));
  return 0;
}
