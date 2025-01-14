#include <iostream>
#include <fstream>
#include <string>

int countStrReductions(const std::string &line)
{ // Returns the number of escaped characters in a line.
  size_t index = 0;
  int reduction = 2;

  while ((index = line.find('\\', index)) != std::string::npos)
  {
    // std::clog << index << ' ';
    // split into cases
    switch (line[index + 1])
    {
    case '\\':
      ++reduction;
      ++index; // increment by two, when added to below 
      break;
    case '\"':
      ++reduction;
      break;
    case 'x':
      reduction += 3;
      break;
    }
    ++index;
  }

  // std::clog << '\n';

  return reduction;
}

int part1(std::istream &is)
{
  int acc = 0;
  std::string line;

  while (std::getline(is, line))
  {
    // std::clog << line << '\n';
    // 2 for " " at start & end
    acc += countStrReductions(line);
  }

  return acc;
}

int countStrElongations(const std::string &line)
{ // Count how many times we will need to elongate the string.
  size_t index = 0;
  int acc = 2; // adds two more "

  for (auto &c : line)
  {
    switch (c)
    {
    case '\\':
      ++acc;
      break;
    case '"':
      ++acc;
      break;
    }
  }

  return acc;
}

int part2(std::istream &is)
{
  std::string line;
  int acc = 0;  

  while (std::getline(is, line))
  {
    acc += countStrElongations(line);
  }

  return acc;
}

int main()
{
  std::ifstream is("2015/day8/input");
  // std::ifstream is("2015/day8/fake-input");

  if (is.is_open())
  {
    // pass
    std::cout << "Part 1: " << part1(is) << '\n';

    // reset is
    is.clear();
    is.seekg(0, std::ios::beg);

    std::cout << "Part 2: " << part2(is) << '\n';
  }
  else
  {
    std::cout << "Error opening file.\n";
  }

  return 0;
}
