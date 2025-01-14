#include <fstream>
#include <iostream>
#include <istream>
#include <sstream>
#include <string>
#include <tuple>

std::tuple<int, int, int> parseLine(const std::string& s) {
  // parses a line LxWxH into (L,W,H)
  const char delimiter = 'x';
  std::string sPrime = "";

  for (auto& c : s) {
    if (c == delimiter) {
      sPrime += ' ';
    } else {
      sPrime += c;
    }
  }

  int i, j, k;
  std::stringstream ss(sPrime);
  ss >> i >> j >> k;

  return {i, j, k};
}

int min(const int& a, const int& b, const int& c) {
  if (a < b && a < c) {
    return a;
  } else if (b < c) {
    return b;
  } else {
    return c;
  }
  return 0;
}

int wrappingArea(const int& l, const int& w, const int& h) {
  const int a1 = l * w;
  const int a2 = l * h;
  const int a3 = w * h;

  return 2 * (a1 + a2 + a3) + min(a1, a2, a3);
}

void part1() {
  std::ifstream inputFile("input");
  std::ofstream outputFile("output.txt");
  std::string line;
  int totalArea = 0;

  if (inputFile.is_open()) {
    while (std::getline(inputFile, line)) {
      auto [l, w, h] = parseLine(line);
      totalArea += wrappingArea(l, w, h);
    }
    std::cout << totalArea;
    outputFile << totalArea;
  } else {
    std::cout << "File opening failed.";
    outputFile << "File opening failed.";
  }
  std::cout << '\n';
}

int ribbonLength(const int& l, const int& w, const int& h) {
  const int peri1 = 2 * (l + w);
  const int peri2 = 2 * (l + h);
  const int peri3 = 2 * (w + h);

  return l * w * h + min(peri1, peri2, peri3);
}

void part2() {
  std::ifstream inputFile("input");
  std::ofstream outputFile("output.txt");
  std::string line;
  int totalLen = 0;

  if (inputFile.is_open()) {
    while (std::getline(inputFile, line)) {
      auto [l, w, h] = parseLine(line);
      totalLen += ribbonLength(l, w, h);
    }
    std::cout << totalLen;
    outputFile << totalLen;
  } else {
    std::cout << "File opening failed.";
    outputFile << "File opening failed.";
  }
  std::cout << '\n';
}

int main() {
  part1();
  part2();
  return 0;
}
