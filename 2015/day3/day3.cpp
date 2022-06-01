#include <algorithm>
#include <fstream>
#include <iostream>
#include <istream>
#include <sstream>
#include <string>
#include <tuple>
#include <vector>

void part1() {
  std::ifstream inputFile("input");
  std::ofstream outputFile("output.txt");
  std::string line;

  int current_pos_x = 0;  // initialise at origin
  int current_pos_y = 0;  // initialise at origin
  std::vector<std::tuple<int, int>> visited{std::make_tuple(
      current_pos_x, current_pos_y)};  // keep track of all places visited
  int delivered = 1;                   // number of houses delivered to

  if (inputFile.is_open()) {
    while (std::getline(inputFile, line)) {
      for (auto& c : line) {
        if (c == '<') {
          --current_pos_x;
        } else if (c == '>') {
          ++current_pos_x;
        } else if (c == '^') {
          ++current_pos_y;
        } else if (c == 'v') {
          --current_pos_y;
        }

        if (std::find(visited.begin(), visited.end(),
                      std::make_tuple(current_pos_x, current_pos_y)) ==
            visited.end()) {
          // if haven't visited yet, increment delivered counter and push back
          // to visited vector
          ++delivered;
          visited.push_back(std::make_tuple(current_pos_x, current_pos_y));
        }
      }
    }
  } else {
    std::cout << "File opening failed.";
  }
  outputFile << "Unique deliveries (pt1): " << delivered;
  std::cout << "Unique deliveries (pt1): " << delivered;

  outputFile << '\n';
  std::cout << '\n';
}

void part2() {
  std::ifstream inputFile("input");
  std::ofstream outputFile("output.txt");
  std::string line;

  // first position is Santa's, second is RoboSanta's
  int current_pos_x[2] = {0, 0};
  int current_pos_y[2] = {0, 0};
  std::vector<std::tuple<int, int>> visited{
      std::make_tuple(current_pos_x[0],
                      current_pos_y[0])};  // keep track of all places visited;
  int delivered = 1;                       // number of houses delivered to
  int counter =
      0;  // keep track whether it is Santa's or RoboSanta's turn to move

  if (inputFile.is_open()) {
    while (std::getline(inputFile, line)) {
      for (auto& c : line) {
        counter ^= 1;  // flip between 0 and 1
        // Note that it doesn't actually matter whether Santa or RoboSanta moves
        // first.
        if (c == '<') {
          --current_pos_x[counter];
        } else if (c == '>') {
          ++current_pos_x[counter];
        } else if (c == '^') {
          ++current_pos_y[counter];
        } else if (c == 'v') {
          --current_pos_y[counter];
        }

        if (std::find(visited.begin(), visited.end(),
                      std::make_tuple(current_pos_x[counter],
                                      current_pos_y[counter])) ==
            visited.end()) {
          // if haven't visited yet, increment delivered counter and push back
          // to visited vector
          ++delivered;
          visited.push_back(
              std::make_tuple(current_pos_x[counter], current_pos_y[counter]));
        }
      }
    }
  } else {
    std::cout << "File opening failed.";
  }
  outputFile << "Unique deliveries (pt2): " << delivered;
  std::cout << "Unique deliveries (pt2): " << delivered;

  outputFile << '\n';
  std::cout << '\n';
}

int main() {
  part1();
  part2();
  return 0;
}
