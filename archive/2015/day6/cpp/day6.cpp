#include <fstream>
#include <iostream>
#include <string>
#include "Grid.hpp"

void process_command(Grid& grid, std::string inputString) {
  // Process commands for part 1.

  // inputString format: "command firstCoords through secondCoords"
  std::string command;       // One of: "turn on" "turn off" "toggle"
  std::string firstCoords;   // Format: "rowStart,colStart"
  std::string secondCoords;  // Format: "rowEnd,colEnd"

  // Locate positions in string
  size_t firstCoordsPos = inputString.find_first_of("1234567890");
  size_t throughPos = inputString.find("through");
  size_t secondCoordsPos = inputString.find_first_of("1234567890", throughPos);

  // Copy substrings into variables for further processing.
  // -1 for command and firstCoords to remove space
  command = inputString.substr(0, firstCoordsPos - 1);
  firstCoords =
      inputString.substr(firstCoordsPos, throughPos - firstCoordsPos - 1);
  secondCoords = inputString.substr(secondCoordsPos);

  size_t firstComma = firstCoords.find_first_of(",");
  size_t secondComma = secondCoords.find_first_of(",");

  size_t rowStart = std::stoi(firstCoords.substr(0, firstComma));
  size_t rowEnd = std::stoi(secondCoords.substr(0, secondComma));
  size_t colStart = std::stoi(firstCoords.substr(firstComma + 1));
  size_t colEnd = std::stoi(secondCoords.substr(secondComma + 1));

  // Debug:
  // . after string to mark end of string
  // std::cerr << "command:\t" << command << ".\n";
  // std::cerr << "first coords:\t" << firstCoords << ".\n";
  // std::cerr << "second coords:\t" << secondCoords << ".\n";

  if (command == "turn on") {
    // std::cerr << "turning on";
    grid.turnOn(rowStart, rowEnd, colStart, colEnd);
  } else if (command == "turn off") {
    // std::cerr << "turning off";
    grid.turnOff(rowStart, rowEnd, colStart, colEnd);
  } else if (command == "toggle") {
    // std::cerr << "toggling";
    grid.toggle(rowStart, rowEnd, colStart, colEnd);
  } else {
    std::cerr << "Invalid command:\t" << command << '\n';
  }
}

int part1(Grid& grid, std::ifstream& is) {
  /* Do part 1. Precondition: is stream is open. */
  std::string line;

  while (std::getline(is, line)) {
    process_command(grid, line);
  }

  return grid.sum();
}

void process_command_2(Grid& grid, std::string inputString) {
  // Process commands for part 2.

  // inputString format: "command firstCoords through secondCoords"
  std::string command;       // One of: "turn on" "turn off" "toggle"
  std::string firstCoords;   // Format: "rowStart,colStart"
  std::string secondCoords;  // Format: "rowEnd,colEnd"

  // Locate positions in string
  size_t firstCoordsPos = inputString.find_first_of("1234567890");
  size_t throughPos = inputString.find("through");
  size_t secondCoordsPos = inputString.find_first_of("1234567890", throughPos);

  // Copy substrings into variables for further processing.
  // -1 for command and firstCoords to remove space
  command = inputString.substr(0, firstCoordsPos - 1);
  firstCoords =
      inputString.substr(firstCoordsPos, throughPos - firstCoordsPos - 1);
  secondCoords = inputString.substr(secondCoordsPos);

  size_t firstComma = firstCoords.find_first_of(",");
  size_t secondComma = secondCoords.find_first_of(",");

  size_t rowStart = std::stoi(firstCoords.substr(0, firstComma));
  size_t rowEnd = std::stoi(secondCoords.substr(0, secondComma));
  size_t colStart = std::stoi(firstCoords.substr(firstComma + 1));
  size_t colEnd = std::stoi(secondCoords.substr(secondComma + 1));

  // Debug:
  // . after string to mark end of string
  // std::cerr << "command:\t" << command << ".\n";
  // std::cerr << "first coords:\t" << firstCoords << ".\n";
  // std::cerr << "second coords:\t" << secondCoords << ".\n";

  if (command == "turn on") {
    // std::cerr << "turning on";
    grid.turnUp(rowStart, rowEnd, colStart, colEnd);
  } else if (command == "turn off") {
    // std::cerr << "turning off";
    grid.turnDown(rowStart, rowEnd, colStart, colEnd);
  } else if (command == "toggle") {
    // std::cerr << "toggling";
    grid.turnUpUp(rowStart, rowEnd, colStart, colEnd);
  } else {
    std::cerr << "Invalid command:\t" << command << '\n';
  }
}

int part2(Grid& grid, std::ifstream& is) {
  /* Do part 1. Precondition: is stream is open. */
  std::string line;

  while (std::getline(is, line)) {
    process_command_2(grid, line);
  }

  return grid.sum();
}

int main() {
  Grid grid = Grid();

  std::ifstream inputFile("2015/day6/cpp/input");

  // Debug:
  // std::string s("turn on 887,9 through 959,629");
  // process_command(grid, s);

  if (inputFile.is_open()) {
    // std::cout << part1(grid, inputFile);
    std::cout << part2(grid, inputFile);
  } else {
    std::clog << "Failed to open file.";
  }

  return 0;
}
