#include "../../utils/utils.cpp"
#include "json.hpp"
#include <fstream>
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

#include "json.cpp"

int part1(json::value val) {
  int acc = 0;
  if (val.type == json::value::number) {
    acc += val.data.i;
  } else if (val.type == json::value::array) {
    for (auto &x : *val.data.ap) {
      acc += part1(x);
    }
  } else if (val.type == json::value::object) {
    for (auto &x : *val.data.op) {
      acc += part1(x.second);
    }
  }
  return acc;
}

bool object_contains_red(json::object_t o) {
  bool red = false;
  for (auto &x : o) {
    std::string k;
    json::value v;
    std::tie(k, v) = x;
    if (k == "red" || (v.type == json::value::string && *v.data.sp == "red")) {
      red = true;
      break;
    }
  }
  return red;
}

int part2(json::value val) {
  int acc = 0;
  if (val.type == json::value::number) {
    acc += val.data.i;
  } else if (val.type == json::value::array) {
    for (auto &x : *val.data.ap) {
      acc += part2(x);
    }
  } else if (val.type == json::value::object) {
    for (auto &x : *val.data.op) {
      auto &v = x.second;
      if (v.type == json::value::string && *v.data.sp == "red") {
        return 0;
      }
      acc += part2(v);
    }
  }
  return acc;
}

int main() {
  std::ifstream is;
  // handle_file_stream(is, "2015/day12/test-input.json");
  handle_file_stream(is, "input.json");
  std::string input;
  std::getline(is, input);
  // std::cout << "INPUT IS " << input << std::endl;
  json::value json = json::parse::parse_value(input);
  report_results(part1(json), part2(json));
  return 0;
}
