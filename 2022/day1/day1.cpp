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
        std::cerr << "Updating maxElf " << maxElf << std::endl;
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

int main()
{
  std::ifstream input("input");

  // Verify opened correctly
  if (!input.is_open())
  {
    std::cerr << "Input file not opened correctly" << std::endl;
    exit(1);
  }

  std::cout << "Part 1: " << part1(input);

  return 0;
}
