#include "../../utils/utils.cpp"
#include "permutations.cpp"
#include <fstream>
#include <iostream>
#include <map>
#include <string>
#include <vector>

// #define TEST
#ifdef TEST
#define FILE "2015/day13/test-input"
#else
#define FILE "2015/day13/input"
#endif

typedef std::map<std::string, std::map<std::string, int>> seating_score_map;

seating_score_map parse_file(std::istream &is) {
  seating_score_map ssm;
  std::string line;
  while (std::getline(is, line)) {
    int i = line.find(' ');
    std::string name1 = line.substr(0, i);
    std::string name2;
    int score;
    bool negative;
    i = skip_n_words(line, 1, i + 1); // gain/lose
    // temporarily use name2
    name2 = line.substr(i, 4);
    negative = name2 == "lose";
    i = skip_n_words(line, 1, i); // score
    score = std::stoi(line.substr(i, std::string::npos));
    if (negative)
      score = -score;
    i = skip_n_words(line, 7, i); // second name
    name2 = line.substr(i, line.length() - i - 1);
    ssm[name1][name2] = score;
  }
  return ssm;
}

// Since % is not actually modulus but remainder </angry>
inline int modulus(int a, int b) {
  int c = a % b;
  if (c < 0) {
    c += b;
  }
  return c;
}

int score_seating_arrangement(const seating_score_map &ssm,
                              std::vector<std::string> seating_arrangement) {
  int score = 0;
  int n = seating_arrangement.size();
  for (int i = 0; i < n; ++i) {
    auto &map_at_person = ssm.at(seating_arrangement.at(i));
    score += map_at_person.at(seating_arrangement.at(modulus(i - 1, n)));
    score += map_at_person.at(seating_arrangement.at(modulus(i + 1, n)));
  }
  return score;
}

// Input is small enough that I'll just brute force it; only 7! = 5040
// combinations
int part1(const seating_score_map &ssm) {
  int best_score = 0;
  std::vector<std::string> names;
  for (auto &pair : ssm) {
    names.push_back(pair.first);
  }
  std::string fixed_name = names.at(0);
  names.erase(names.begin());
  std::vector<std::vector<int>> permutations =
      generate_permutations_wrapper(names.size());
  for (auto &permutation : permutations) {
    std::vector<std::string> seating_arrangement;
    for (auto &i : permutation) {
      seating_arrangement.push_back(names.at(i));
    }
    seating_arrangement.push_back(fixed_name);
    int score = score_seating_arrangement(ssm, seating_arrangement);
    if (score > best_score) {
      best_score = score;
    }
  }
  return best_score;
}

int score_seating_arrangement2(const seating_score_map &ssm,
                               std::vector<std::string> seating_arrangement) {
  int score = 0;
  int n = seating_arrangement.size();
  for (int i = 1; i < n; ++i) {
    score +=
        ssm.at(seating_arrangement.at(i)).at(seating_arrangement.at(i - 1));
    score +=
        ssm.at(seating_arrangement.at(i - 1)).at(seating_arrangement.at(i));
  }
  return score;
}

int part2(const seating_score_map &ssm) {
  int best_score = 0;
  std::vector<std::string> names;
  for (auto &pair : ssm) {
    names.push_back(pair.first);
  }
  std::vector<std::vector<int>> permutations =
      generate_permutations_wrapper(names.size());
  for (auto &permutation : permutations) {
    std::vector<std::string> seating_arrangement;
    for (auto &i : permutation) {
      seating_arrangement.push_back(names.at(i));
    }
    int score = score_seating_arrangement2(ssm, seating_arrangement);
    if (score > best_score) {
      best_score = score;
    }
  }
  return best_score;
}

int main() {
  std::ifstream is;
  handle_file_stream(is, FILE);
  seating_score_map ssm = parse_file(is);
  report_results(part1(ssm), part2(ssm));
  return 0;
}
