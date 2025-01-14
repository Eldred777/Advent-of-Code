#include <fstream>  // file writing
#include <iostream>
#include <string>

void part1() {
  std::ifstream inputFile("input");
  std::ofstream outputFile;
  std::string line;  // string to hold current line

  int acc = 0;  // accumulator for current state

  if (inputFile.is_open()) {  // make sure file opens properly
    while (std::getline(inputFile, line)) {
      for (auto& c : line) {  // iterate over line and add up floor changes
        if (c == '(') {
          ++acc;
        } else if (c == ')') {
          --acc;
        }
      }
      std::cout << acc;
    }
    inputFile.close();
  } 
}

void part2() {
  std::ifstream inputFile("input");
  std::ofstream outputFile;
  std::string line;  // string to hold current line

  int ind = 0;  // index for position in input string
  int acc = 0;  // accumulator for current state

  outputFile.open("output.txt");

  if (inputFile.is_open()) {  // make sure file opens properly
    while (std::getline(inputFile, line)) {
      for (auto& c : line) {  // iterate over line and add up floor changes
        ++ind;

        if (c == '(') {
          ++acc;
        } else if (c == ')') {
          --acc;
        }

        if (acc == -1) {
          std::cout << ind;
          outputFile << ind;
          break;
        }
      }
    }
    inputFile.close();
  } else {  // exit gracefully if file does not open properly
    std::cout << "Unable to open file";
    outputFile << "Unable to open file";
  }
  std::cout << '\n';
}

int main() {
  part1();
  part2();

  return 0;
}
