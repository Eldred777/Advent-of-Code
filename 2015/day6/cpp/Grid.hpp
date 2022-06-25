#include <cstddef>

typedef char arrElem;
#define arraySize 1000

class Grid {
 private:
  arrElem values[arraySize][arraySize] = {0};

  // Part 1 helper commands 
  Grid& toggleRow(size_t rowNo, size_t colStart, size_t colEnd);
  Grid& setRow(size_t rowNo, size_t colStart, size_t colEnd, arrElem target);
  Grid& setBox(size_t rowStart,
               size_t rowEnd,
               size_t colStart,
               size_t colEnd,
               arrElem target);

 public:
  Grid() = default;

  // Part 1 commands
  Grid& toggle(size_t rowStart, size_t rowEnd, size_t colStart, size_t colEnd);
  Grid& turnOff(size_t rowStart, size_t rowEnd, size_t colStart, size_t colEnd);
  Grid& turnOn(size_t rowStart, size_t rowEnd, size_t colStart, size_t colEnd);

  // Part 2 commands
  Grid& turnDown(size_t rowStart,
                 size_t rowEnd,
                 size_t colStart,
                 size_t colEnd);
  Grid& turnUp(size_t rowStart, size_t rowEnd, size_t colStart, size_t colEnd);
  Grid& turnUpUp(size_t rowStart,
                 size_t rowEnd,
                 size_t colStart,
                 size_t colEnd);

  int sum();
};
