#include <fstream>
#include <iostream>
#include <string>
#include <unordered_map>

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
  // std::cout << "Wire name:" << wireName << "::";

  try
  {
    std::string circuitInput = map.at(wireName);

    std::string circuit;
    size_t foundIndex = std::string::npos;
    std::string left;
    std::string command;
    std::string right;
    int i_left;
    int i_right;
    int i_circuit;

    // std::cerr << circuitInput << ".\n";

    // If no spaces, then direct assignment
    if (circuitInput.find_first_of(" ") == std::string::npos)
    {
      if (circuitInput.find_first_of("1234567890") != std::string::npos)
      { // CASE: number
        circuit = circuitInput;
      }
      else
      { // CASE: another wire
        circuit = parseCircuit(map, circuitInput);
      }
    }
    else if ((foundIndex = circuitInput.find("NOT")) != std::string::npos)
    { // NOTE! the inline assignment to foundIndex.
      // circuit = "(NOT " + parseCircuit(map, circuitInput.substr(4)) + ")";
      
      left = parseCircuit(map, circuitInput.substr(4)); // take value
      i_left = std::stoi(left);                         // convert to integer
      i_circuit = !i_left;                              // simulate
      circuit = i_circuit;                              // convert to string
    }
    else if ((foundIndex = circuitInput.find("SHIFT")) != std::string::npos)
    {
      // std::cerr << "TOKENS: " << circuitInput.substr(0, foundIndex - 2) << ".\n";

      // todo: check whether these left, right are correct
      left = parseCircuit(map, circuitInput.substr(0, foundIndex - 2));
      right = parseCircuit(map, circuitInput.substr(foundIndex + 7));

      i_left = std::stoi(left); // convert to integer
      i_right = std::stoi(right);

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
      circuit = i_circuit;
    }
    else if ((foundIndex = circuitInput.find("AND")) != std::string::npos)
    {
      // circuit = "(" + parseCircuit(map, circuitInput.substr(0, foundIndex - 1)) + " AND " + parseCircuit(map, circuitInput.substr(foundIndex + 4)) + ")";

      // todo: check these left and right
      left = parseCircuit(map, circuitInput.substr(0, foundIndex - 1));
      right = parseCircuit(map, circuitInput.substr(foundIndex + 4));

      i_left = std::stoi(left); // convert to integer
      i_right = std::stoi(right);

      i_circuit = i_left & i_right;

      circuit = i_circuit;
    }
    else if ((foundIndex = circuitInput.find("OR")) != std::string::npos)
    {
      // std::cerr << "TOKENS: " << circuitInput.substr(0, foundIndex - 1) << "," << circuitInput.substr(foundIndex + 3) << ".\n";
      circuit = "(" + parseCircuit(map, circuitInput.substr(0, foundIndex - 1)) + " OR " + parseCircuit(map, circuitInput.substr(foundIndex + 3)) + ")";

      // todo: check these left and right
      left = circuitInput.substr(0, foundIndex - 1);
      right = circuitInput.substr(foundIndex + 3);

      i_left = std::stoi(left); // convert to integer
      i_right = std::stoi(right);

      i_circuit = i_left | i_right; // simulate circuit

      circuit = i_circuit;
    }

    // todo simulate circuit before updating map --- want a numerical value at the end

    map[wireName] = circuit; // update map
    return circuit;
  }
  catch (const std::out_of_range &e)
  { // Wire name not in map; return wire name.
    std::cerr << "Wire name not in map: " << wireName << '\n';
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

int main()
{
  std::ifstream inputFile("2015/day7/input.txt");
  // std::ifstream inputFile("2015/day7/fake-input.txt");

  std::string p1 = "null";

  if (inputFile.is_open())
  {
    p1 = part1(inputFile);
  }
  else
  {
    std::cerr << "Error opening file.\n";
  }

  std::cout << "Part 1: " << p1 << '\n';

  return 0;
}
