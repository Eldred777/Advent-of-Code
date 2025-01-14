#include <assert.h>
#include <numeric>
#include <vector>

// Generates permutations of `n` elements, {1..n}
// Expect n! such permutations.
void generate_permutations(std::vector<std::vector<int>> &result_list,
                           std::vector<int> current,
                           std::vector<int> remaining) {
  if (remaining.size() == 0) { // base case
    result_list.push_back(current);
    return;
  }
  for (int i = 0; i < remaining.size(); ++i) {
    auto r = remaining; // copy
    r.erase(r.begin() + i);
    auto c = current;
    c.push_back(remaining.at(i));
    generate_permutations(result_list, c, r);
  }
}

int fact(int n) {
  int acc = 1;
  for (int i = 1; i <= n; ++i) {
    acc *= i;
  }
  return acc;
}

std::vector<std::vector<int>> generate_permutations_wrapper(int n) {
  std::vector<std::vector<int>> result;
  std::vector<int> init_remaining(n);
  std::iota(init_remaining.begin(), init_remaining.end(), 0);
  auto l = std::vector<int>{};
  generate_permutations(result, l, init_remaining);
  assert(result.size() == fact(n));
  return result;
}
