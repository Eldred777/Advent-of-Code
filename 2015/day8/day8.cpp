#include <iostream>
#include <fstream>
#include <string>

int countStrReductions(const std::string &line)
{ // Returns the number of escaped characters in a line.
  size_t index = 0;
  int reduction = 2;

  while ((index = line.find('\\', index + 1)) != std::string::npos)
  {
    // split into cases
    switch (line[index + 1])
    {
    case '\\':
      ++reduction;
      index += 2;
      break;
    case '\"':
      ++reduction;
      index += 2;
      break;
    case 'x':
      reduction += 3;
      index += 4;
      break;
    }
  }

  return reduction;
}

int part1(std::istream &is)
{ // todo
  int codeLength = 0;
  int strLength = 0;
  std::string line;

  while (std::getline(is, line))
  {
    // 2 for " " at start & end
    codeLength += line.length();
    strLength += countStrReductions(line);
  }

  return codeLength - strLength;
}

int part2(std::istream &is)
{ // todo
  return 0;
}

int main()
{
  // std::ifstream is("2015/day8/input");
  std::ifstream is("2015/day8/fake-input");

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
