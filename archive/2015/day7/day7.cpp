#include <fstream>
#include <iostream>
#include <string>
#include <unordered_map>

inline int custom_not(int i)
{
  return 65535 ^ i;
}

inline std::string rstrip(std::string &s)
{
  if (s[s.size() - 1] == '\r')
  {
    s.pop_back();
  }
  return s;
}

void constructMapByLine(std::unordered_map<std::string, std::string> &map,
                        const std::string &line)
{
  /* Construct the preliminary map for the circuit. */
  size_t arrowPos = line.find("->");                // index of arrow
  std::string target = line.substr(arrowPos + 3);   // name of target
  std::string input = line.substr(0, arrowPos - 1); // circuit input to target
  rstrip(target);

  map[target] = input; // update map with new entry
}

std::string
parseCircuit(std::unordered_map<std::string, std::string> &map,
             const std::string &wireName)
{
  /**
   * @brief Parses circuit input until fully simplified. Uses the map to look
   * up the next step down the chain.
   * The map is updated with the simplified (numerically simulated) expression.
   *
   * Commands:
   * - NOT
   * - RSHIFT
   * - LSHIFT
   * - direct assignment e.g. x -> y
   * |- in this case only, do not insert brackets
   * - AND
   * - OR
   *
   */

  try
  {
    if (wireName.find_first_of(" ") == std::string::npos && wireName.find_first_of("1234567890") != std::string::npos)
    { // Wire name is a number.
      return wireName;
    }

    std::string circuit; // final result
    std::string circuitInput = map.at(wireName);
    size_t foundIndex = std::string::npos;
    std::string left;
    std::string right;
    int i_left;
    int i_right;
    int i_circuit;
    bool overwrite = true; // Bool that goes false only if we do not want to overwrite the map with the found value.

    // If no spaces, then direct assignment
    if (circuitInput.find_first_of(" ") == std::string::npos)
    {
      if (circuitInput.find_first_of("1234567890") != std::string::npos)
      { // CASE: number
        circuit = circuitInput;
        overwrite = false;
      }
      else
      { // CASE: another wire
        circuit = parseCircuit(map, circuitInput);
      }
    }
    else if ((foundIndex = circuitInput.find("NOT")) != std::string::npos)
    {                                                   // NOTE! the inline assignment to foundIndex.
      left = parseCircuit(map, circuitInput.substr(4)); // take value
      i_left = std::stoi(left);                         // convert to integer
      i_circuit = custom_not(i_left);                   // simulate
      circuit = std::to_string(i_circuit);              // convert to string
    }
    else if ((foundIndex = circuitInput.find("SHIFT")) != std::string::npos)
    {
      left = parseCircuit(map, circuitInput.substr(0, foundIndex - 2));
      right = parseCircuit(map, circuitInput.substr(foundIndex + 6));

      i_left = std::stoi(left); // convert to integer
      i_right = std::stoi(right);

      // cases: left shift or right shift
      if (circuitInput[foundIndex - 1] == 'L')
      {
        // simulate left shift
        i_circuit = i_left << i_right;
      }
      else
      {
        // simulate right shift
        i_circuit = i_left >> i_right;
      }

      // convert to string
      circuit = std::to_string(i_circuit); // convert to string
    }
    else if ((foundIndex = circuitInput.find("AND")) != std::string::npos)
    {
      left = parseCircuit(map, circuitInput.substr(0, foundIndex - 1));
      right = parseCircuit(map, circuitInput.substr(foundIndex + 4));

      i_left = std::stoi(left); // convert to integer
      i_right = std::stoi(right);

      i_circuit = i_left & i_right; // simulate

      circuit = std::to_string(i_circuit); // convert to string
    }
    else if ((foundIndex = circuitInput.find("OR")) != std::string::npos)
    {
      left = parseCircuit(map, circuitInput.substr(0, foundIndex - 1));
      right = parseCircuit(map, circuitInput.substr(foundIndex + 3));

      i_left = std::stoi(left); // convert to integer
      i_right = std::stoi(right);

      i_circuit = i_left | i_right; // simulate circuit

      circuit = std::to_string(i_circuit); // convert to string
    }

    if (overwrite)
    {
      map[wireName] = circuit; // update map
    }
    return circuit;
  }
  catch (const std::out_of_range &e)
  { // Wire name not in map; return wire name.
    std::cerr << "Wire name not in map: " << wireName << ".\n";
    return wireName;
  }
}

std::string
part1(std::istream &is)
{
  const std::string targetWire = "a";

  // Put all circuit inputs into a map.
  std::unordered_map<std::string, std::string> map = {};
  std::string line;
  while (std::getline(is, line))
  {
    constructMapByLine(map, line);
  }

  return parseCircuit(map, targetWire);
}

std::string
part2(std::istream &is)
{
  const std::string targetWire = "a";
  const std::string targetWire2 = "b";
  std::string output;

  // Put all circuit inputs into a map.
  std::unordered_map<std::string, std::string> map = {};
  std::string line;
  while (std::getline(is, line))
  {
    constructMapByLine(map, line);
  }

  // store output
  output = parseCircuit(map, targetWire);

  // reset input file
  is.clear();                 // clear fail and eof bits
  is.seekg(0, std::ios::beg); // back to the start!
  // reset map
  while (std::getline(is, line))
  {
    constructMapByLine(map, line);
  }

  map[targetWire2] = output; // overwrite map

  return parseCircuit(map, targetWire);
}

int main()
{
  std::ifstream inputFile("2015/day7/input.txt");
  // std::ifstream inputFile("2015/day7/fake-input.txt");

  if (inputFile.is_open())
  {
    std::cout << "Part 1: " << part1(inputFile) << '\n';

    // reset input file
    inputFile.clear();                 // clear fail and eof bits
    inputFile.seekg(0, std::ios::beg); // back to the start!

    std::cout << "Part 2: " << part2(inputFile) << '\n';
  }
  else
  {
    std::cerr << "Error opening file.\n";
  }

  return 0;
}
