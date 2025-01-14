#include <fstream>
#include <iostream>
#include <map>
#include <set>
#include <stdexcept>
#include <string>

#include "../../utils/utils.cpp"
void handle_file_stream(std::ifstream&, std::string);
// Not sure why but the error checker thingy can't find this fn so declaration

typedef std::string node_label;
typedef std::map<node_label, std::map<node_label, int> > distance_map_t;
typedef std::map<node_label, int> distance_map_inner_t;

void update_distance_map(const node_label& first_word,
                         const node_label& second_word,
                         const int distance,
                         distance_map_t& distance_map) {
  try {
    distance_map_inner_t& distance_map_inner = distance_map.at(first_word);
    distance_map_inner.insert(std::pair(second_word, distance));
  } catch (const std::out_of_range&) {
    distance_map_inner_t x;
    x.insert(std::pair(second_word, distance));
    distance_map.insert(std::pair(first_word, x));
  }
}

void parse_line(std::string& line,
                distance_map_t& distance_map,
                std::set<std::string>& node_labels) {
  size_t index_first_space = line.find(' ');
  if (index_first_space == std::string::npos) {
    std::cerr << "Failed to find first space for line: " << line << std::endl;
    exit(1);
  }
  std::string first_word = line.substr(0, index_first_space);

  // " to _"
  //  ^...^
  size_t index_start_of_second_word = index_first_space + 4;
  size_t index_end_of_second_word = line.find(' ', index_start_of_second_word);
  size_t index_diff = index_end_of_second_word - index_start_of_second_word;
  std::string second_word = line.substr(index_start_of_second_word, index_diff);

  size_t index_equal = line.find('=');
  int distance = std::stoi(line.substr(index_equal + 2));

  update_distance_map(first_word, second_word, distance, distance_map);
  update_distance_map(second_word, first_word, distance, distance_map);
  // This second line is a hacky way to recognise that the distances are not
  // direction dependent; initially I had worked through this problem assuming
  // it was __direc
  node_labels.insert(first_word);
  node_labels.insert(second_word);
}

void parse_file(std::istream& is,
                distance_map_t& distance_map,
                std::set<std::string>& node_labels) {
  std::string line;
  while (std::getline(is, line)) {
    parse_line(line, distance_map, node_labels);
  }
}

void part_1_traverse_step(const distance_map_t& distance_map,
                          std::set<node_label>& traversed_nodes,
                          const node_label& source_node,
                          const distance_map_inner_t& target_nodes,
                          const int& num_nodes,
                          int& min_dist,
                          const int current_distance) {
  traversed_nodes.insert(source_node);
  for (const auto& end_node : distance_map.at(source_node)) {
    int tentative_distance = current_distance + end_node.second;
    if (tentative_distance > min_dist)  // Can rule this out.
      continue;
    else if (traversed_nodes.count(end_node.first))  // Already encountered
      continue;
    else if (traversed_nodes.size() == num_nodes - 1)
      // All encountered. Update `min_dist`.
      min_dist =
          (min_dist < tentative_distance) ? min_dist : tentative_distance;
    else
      part_1_traverse_step(distance_map, traversed_nodes, end_node.first,
                           distance_map.at(end_node.first),  // See below.
                           num_nodes, min_dist, tentative_distance);
  }
  traversed_nodes.erase(source_node);  // backtracking
}

int part1(distance_map_t& distance_map, std::set<std::string>& node_labels) {
  int min_dist = INT_MAX;
  int num_nodes = node_labels.size();

  std::set<node_label> traversed_nodes;
  for (auto& start : distance_map) {
    part_1_traverse_step(distance_map, traversed_nodes, start.first,
                         start.second, num_nodes, min_dist, 0);
  }

  std::cout << "Part 1: " << min_dist << std::endl;

  return 0;
}

void part_2_traverse_step(const distance_map_t& distance_map,
                          std::set<node_label>& traversed_nodes,
                          const node_label& source_node,
                          const distance_map_inner_t& target_nodes,
                          const int& num_nodes,
                          int& max_dist,
                          const int current_distance) {
  traversed_nodes.insert(source_node);
  for (const auto& end_node : distance_map.at(source_node)) {
    int tentative_distance = current_distance + end_node.second;
    if (traversed_nodes.count(end_node.first))  // Already encountered
      continue;
    else if (traversed_nodes.size() == num_nodes - 1)
      // All encountered. Update `max_dist`.
      max_dist =
          (max_dist > tentative_distance) ? max_dist : tentative_distance;
    else
      part_2_traverse_step(distance_map, traversed_nodes, end_node.first,
                           distance_map.at(end_node.first),  // See below.
                           num_nodes, max_dist, tentative_distance);
  }
  traversed_nodes.erase(source_node);  // backtracking
}

int part2(distance_map_t& distance_map, std::set<std::string>& node_labels) {
  int max_dist = 0;
  int num_nodes = node_labels.size();

  std::set<node_label> traversed_nodes;
  for (auto& start : distance_map) {
    part_2_traverse_step(distance_map, traversed_nodes, start.first,
                         start.second, num_nodes, max_dist, 0);
  }

  std::cout << "Part 2: " << max_dist << std::endl;

  return 0;
}

int main() {
  std::ifstream is;
  handle_file_stream(is, std::string("2015/day9/input"));

  distance_map_t distance_map;
  std::set<std::string> node_labels;
  parse_file(is, distance_map, node_labels);

  /* For debugging
  for (auto& x : distance_map) {
    std::clog << x.first << ":" << std::endl;
    for (auto& y : x.second) {
      std::clog << "  " << y.first << " -- " << y.second << std::endl;
    }
  }
   */

  part1(distance_map, node_labels);
  part2(distance_map, node_labels);

  return 0;
}
