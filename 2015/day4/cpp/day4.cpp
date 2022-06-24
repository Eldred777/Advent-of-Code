#include <bitset>
#include <chrono>
#include <iostream>
#include <string>
#include "md5.hpp"

int part1(char input[]) {
  unsigned int i = 1;
  std::string s(input);  // do not mutate this

  while (true) {
    std::string ss = s + std::to_string(i);

    if (md5(ss).rfind("00000", 0) == 0) {
      return i;
    }
    ++i;
  }
}

int part2(char input[]) {
  unsigned int i = 1;
  std::string s(input);  // do not mutate this

  while (true) {
    std::string ss = s + std::to_string(i);

    if (md5(ss).rfind("000000", 0) == 0) {
      return i;
    }
    ++i;
  }
}

int main() {
  auto start = std::chrono::high_resolution_clock::now();

  char input[] = "ckczppom";  // secret key

  std::cout << "Part 1: " << part1(input) << '\n';

  auto stop = std::chrono::high_resolution_clock::now();
  auto duration =
      std::chrono::duration_cast<std::chrono::milliseconds>(stop - start);
  std::cout << "Time taken by function: " << duration.count() << " ms";
  return 0;
}
