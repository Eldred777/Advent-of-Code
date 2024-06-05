#include "../../utils/utils.cpp"
#include <fstream>
#include <iostream>
#include <map>
#include <string>

// ASSUMPTION: only one Sue matches all properties
int part1(std::ifstream &is, std::map<std::string, int> &targetSue) {
  std::string line;
  int sueNum = 0;
  while (std::getline(is, line)) {
    bool next_sue = false;
    int i = skip_n_words(line, 1, 0);
    sueNum = std::stoi(line.substr(i, 3));
    i = skip_n_words(line, 1, i);
    while (i != 0) { // 0 is a sharp edge in `skip_n_words`
      int j = line.find(' ', i);
      std::string property = line.substr(i, j - i - 1);
      i = j + 1;
      j = std::stoi(line.substr(i, std::string::npos));
      if (j != targetSue.at(property)) {
        next_sue = true;
        break;
      }
      i = skip_n_words(line, 1, i);
    }
    if (next_sue) {
      continue;
    } else {
      break;
    }
  }
  return sueNum;
}

bool part2comparison(std::string key, int val,
                     std::map<std::string, int> &targetSue) {
  if (key == "cats" || key == "trees") {
    return val > targetSue.at(key);
  } else if (key == "pomeranians" || key == "goldfish") {
    return val < targetSue.at(key);
  } else {
    return val == targetSue.at(key);
  }
  return false;
}

int part2(std::ifstream &is, std::map<std::string, int> &targetSue) {
  std::string line;
  int sueNum = 0;
  while (std::getline(is, line)) {
    bool next_sue = false;
    int i = skip_n_words(line, 1, 0);
    sueNum = std::stoi(line.substr(i, 3));
    i = skip_n_words(line, 1, i);
    while (i != 0) { // 0 is a sharp edge in `skip_n_words`
      int j = line.find(' ', i);
      std::string property = line.substr(i, j - i - 1);
      i = j + 1;
      j = std::stoi(line.substr(i, std::string::npos));
      if (!part2comparison(property, j, targetSue)) {
        next_sue = true;
        break;
      }
      i = skip_n_words(line, 1, i);
    }
    if (next_sue) {
      continue;
    } else {
      break;
    }
  }
  return sueNum;
}

int main() {
  std::ifstream is;
  handle_file_stream(is, "2015/day16/input");
  std::map<std::string, int> targetSue;
  targetSue["children"] = 3;
  targetSue["cats"] = 7;
  targetSue["samoyeds"] = 2;
  targetSue["pomeranians"] = 3;
  targetSue["akitas"] = 0;
  targetSue["vizslas"] = 0;
  targetSue["goldfish"] = 5;
  targetSue["trees"] = 3;
  targetSue["cars"] = 2;
  targetSue["perfumes"] = 1;
  report_results(part1(is, targetSue), part2(is, targetSue));
  return 0;
}
