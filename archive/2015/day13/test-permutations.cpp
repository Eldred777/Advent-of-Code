#include "permutations.cpp"

#include <iostream>

int main() {
  auto l = generate_permutations_wrapper(4);
  for (auto &permutation : l) {
    for (auto &s : permutation) {
      std::cout << s << ' ';
    }
    std::cout << std::endl;
  }

  return 0;
}
