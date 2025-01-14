#include "../../utils/utils.cpp"
#include <algorithm>
#include <assert.h>
#include <fstream>
#include <iostream>
#include <numeric>
#include <string>
#include <vector>

// #define TEST
#ifdef TEST
constexpr int TARGET_TIME = 1000;
#define FILE "2015/day14/test-input"
#else
constexpr int TARGET_TIME = 2503;
#define FILE "2015/day14/input"
#endif

inline int min(int a, int b) { return a < b ? a : b; }

struct reindeer {
  int flying_speed = 0;
  int flight_duration = 0;
  int rest_duration = 0;

  // part 1
  int distance = 0;
  void simulate();
  // part 2
  void reset_distance() { distance = 0; }
  int points = 0;
  void award() { points++; }
  int current_flight_time = 0;
  int current_rest_time = 0;
  void step();
};

void reindeer::simulate() {
  const int cycle_time = flight_duration + rest_duration;
  const int distance_per_cycle = flying_speed * flight_duration;
  const int num_cycles = TARGET_TIME / cycle_time;
  const int remaining_time = TARGET_TIME - num_cycles * cycle_time;
  distance += num_cycles * distance_per_cycle;
  distance += min(remaining_time, flight_duration) * flying_speed;
  return;
}

// Time evolve by 1 second.
void reindeer::step() {
start:
  if (current_flight_time < flight_duration) {
    distance += flying_speed; // distance per second * 1 second
    current_flight_time++;
  } else if (current_rest_time < rest_duration) {
    current_rest_time++;
  } else {
    current_flight_time = current_rest_time = 0;
    goto start;
  }
  return;
}

reindeer parse_reindeer(std::string s) {
  reindeer reindeer;
  size_t i = 0;
  i = skip_n_words(s, 3, 0);
  reindeer.flying_speed = std::stoi(s.substr(i, std::string::npos));
  i = skip_n_words(s, 3, i + 1);
  reindeer.flight_duration = std::stoi(s.substr(i, std::string::npos));
  i = skip_n_words(s, 7, i + 1);
  reindeer.rest_duration = std::stoi(s.substr(i, std::string::npos));
  return reindeer;
}

int part1(std::vector<reindeer> &reindeers) {
  std::vector<int> ordering(reindeers.size());
  std::iota(ordering.begin(), ordering.end(), 0);
  for (auto &r : reindeers) {
    r.simulate();
  }
  std::sort(ordering.begin(), ordering.end(), [reindeers](int &a, int &b) {
    return reindeers.at(a).distance > reindeers.at(b).distance;
  });
  return reindeers.at(ordering.at(0)).distance;
}

int part2(std::vector<reindeer> &reindeers) {
  for (auto &r : reindeers) {
    r.reset_distance();
  }
  for (int t = 0; t < TARGET_TIME; ++t) {
    for (auto &r : reindeers) {
      r.step();
    }
    // award a star to current leader(s)
    std::vector<int> leaders = {0};
    int max_dist = reindeers.at(0).distance;
    for (int i = 1; i < reindeers.size(); ++i) {
      int &d = reindeers.at(i).distance;
      if (d > max_dist) {
        max_dist = d;
        leaders.clear();
        leaders.push_back(i);
      } else if (d == max_dist) {
        leaders.push_back(i);
      }
    }
    for (auto &leader_idx : leaders) {
      reindeers.at(leader_idx).award();
    }
  }
  // find reindeer with most points
  int max_points = 0;
  for (auto &r : reindeers) {
    if (r.points > max_points) {
      max_points = r.points;
    }
  }
  return max_points;
}

int main() {
  std::ifstream is;
  handle_file_stream(is, FILE);
  std::string line;
  std::vector<reindeer> reindeers;
  while (std::getline(is, line) && line.size()) {
    reindeers.push_back(parse_reindeer(line));
  }
  report_results(part1(reindeers), part2(reindeers));
  return 0;
}
