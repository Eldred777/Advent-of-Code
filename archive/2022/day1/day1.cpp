#include <fstream>
#include <iostream>
#include <string>

int part1(std::istream &is)
{
  int maxElf = 0;
  int currentElf = 0;
  std::string line;

  // read lines of file
  while (std::getline(is, line))
  {
    // Check special case
    if (line == "")
    {
      // Update maxElf
      if (currentElf > maxElf)
      {
        maxElf = currentElf;
      }

      // Reset value for next elf
      currentElf = 0;
    }
    else
    {
      // Convert line to int and add to running total for this elf.
      currentElf += std::stoi(line);
    }
  }

  return maxElf;
}

template <typename T>
class customQueue
{
  T *least;
  T a;
  T b;
  T c;

public:
  void update_least()
  {
    T *tmp;
    // tmp holds least of a, b
    tmp = (a < b) ? &a : &b;
    // least holds smallest of *tmp and c
    least = (*tmp < c) ? tmp : &c;
  }

  customQueue()
  {
    a = 0;
    b = 0;
    c = 0;
    least = &c; // consistent with update_least method, but doesn't matter
  }
  customQueue(T a, T b, T c)
  {
    a = a;
    b = b;
    c = c;
    update_least();
  }

  // Replace the least value with the input if greater.
  void shunt(T input)
  {
    if (input > *least)
    {
      *least = input;
      update_least();
    }
    // else, nothing
  }

  std::string print()
  {
    return std::string("") + a + " " + b + " " + c;
  }

  T sum() { return a + b + c; }
};

int part2(std::istream &is)
{
  customQueue<int> maxElves;
  int currentElf = 0;
  std::string line;

  // read lines of file
  while (std::getline(is, line))
  {
    // Check special case
    if (line == "")
    {
      // Reset value for next elf
      maxElves.shunt(currentElf);
      currentElf = 0;
    }
    else
    {
      // Convert line to int and add to running total for this elf.
      currentElf += std::stoi(line);
    }
  }

  return maxElves.sum();
}

int main()
{
  std::ifstream input("input");

  // Verify opened correctly
  if (!input.is_open())
  {
    std::cerr << "Input file not opened correctly" << std::endl;
    exit(1);
  }

  std::cout << "Part 1: " << part1(input) << std::endl;

  // Reset input
  input.close();
  input.open("input");

  std::cout << "Part 2: " << part2(input);

  // close file when finished
  input.close();

  return 0;
}
