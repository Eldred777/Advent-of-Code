#include <fstream>
#include <iostream>
#include <string>
#include <unordered_set>

/*  Determines how many of a list of strings (given as an input file) are
nice.

Criteria:
- at least 3 vowels
- at least one repetition of letters
- no subsequences of ab cd pq xy
*/
unsigned int part1(std::ifstream& is) {
  std::string line;
  unsigned int niceCount = 0;

  while (std::getline(is, line)) {
    unsigned int vowelCount = 0;
    bool doubleUp = false;
    bool containsBad = false;

    for (auto it = line.begin(); it != line.end(); ++it) {
      // 1. Check if vowel.
      if (std::string("aeiou").find(*(it)) != std::string::npos) {
        ++vowelCount;
      }

      auto itt = it + 1;
      if (itt != line.end()) {
        // Check to make sure looking at next letter isn't going to cause bad
        // things.
        // 2. Check if this character is repeated
        if (*it == *itt && !doubleUp) {
          doubleUp = true;
        }

        // 3. Check if naughty.
        if (*it == 'a' && *itt == 'b') {
          containsBad = true;
        } else if (*it == 'c' && *itt == 'd') {
          containsBad = true;
        } else if (*it == 'p' && *itt == 'q') {
          containsBad = true;
        } else if (*it == 'x' && *itt == 'y') {
          containsBad = true;
        }
      }
    }

    if (vowelCount > 2 && doubleUp == true && containsBad == false) {
      ++niceCount;
    }
  }

  return niceCount;
}

/*  Determines how many of a list of strings (given as an input file) are
nice.

Criteria:
- two repeated pairs of letters, non-overlapping
- pattern of type aba, for a, b arbitrary letters
*/
unsigned int part2(std::ifstream& is) {
  std::string line;
  unsigned int niceCount = 0;
  std::unordered_set<std::string> pairSet = {};

  while (std::getline(is, line)) {
    // Iterate across lines
    // Re-initialise variables.
    pairSet.clear();
    bool skipOnce = false;
    bool repeatedPair = false;
    bool aba = false;

    for (auto it = line.begin(); it != line.end(); ++it) {
      // Iterate across single line
      auto itt = it + 1;
      auto ittt = itt + 1;

      if (itt != line.end()) {
        if (!repeatedPair) {
          // 1. Repeated pairs of letters
          std::string pair = std::string(1, *it) + *itt;

          // If pair found, then two copies exist.
          // Need to skip if *itt = *ittt.
          // Else, insert pair.
          if (pairSet.find(pair) != pairSet.end()) {
            // if pair found in set
            repeatedPair = true;
          } else if (skipOnce) {
            // Special case: previous pair skipped.
            skipOnce = false;
            pairSet.insert(pair);
          } else if (ittt != line.end() && *it == *itt && *itt == *ittt) {
            // Skip since this case will be covered later.
            skipOnce = true;
          } else {
            pairSet.insert(pair);
          }
        }

        // 2. aba pattern
        if (!aba && ittt != line.end() && *it == *ittt) {
          aba = true;
        }
      }
    }

    if (repeatedPair && aba) {
      //// std::cerr << line << std::endl;
      ++niceCount;
    }
  }
  return niceCount;
}

int main() {
  std::ifstream inputFile("input");
  // std::ifstream inputFile("input-fake");

  if (inputFile.is_open()) {
    std::cout << "Part 1: " << part1(inputFile) << std::endl;
  } else {
    std::cerr << "Error: unable to open input file." << std::endl;
  }

  // ------------------------------------
  inputFile.clear();                  // clear fail and eof bits
  inputFile.seekg(0, std::ios::beg);  // go back to start of stream

  if (inputFile.is_open()) {
    std::cout << "Part 2: " << part2(inputFile) << std::endl;
  } else {
    std::cerr << "Error: unable to open input file." << std::endl;
  }

  return 0;
}
