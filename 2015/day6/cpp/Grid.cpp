#include "Grid.hpp"

Grid& Grid::toggleRow(size_t rowNo, size_t colStart, size_t colEnd) {
  /* Pass row array by reference, then toggle from colStart to colEnd
   * inclusive.
   */
  for (size_t i = colStart; i <= colEnd; i++) {
    values[rowNo][i] ^= 1;
  }
  return *this;
}

Grid& Grid::setRow(size_t rowNo,
                   size_t colStart,
                   size_t colEnd,
                   arrElem target) {
  /* Pass row array by reference, then set all elements from colStart to colEnd
   * inclusive.
   */
  for (size_t i = colStart; i <= colEnd; i++) {
    values[rowNo][i] = target;
  }
  return *this;
}

int Grid::sum() {
  /* Computes sum of elements of array, entirely flattened. */
  int acc = 0;  // accumulator
  for (auto& i : values) {
    for (auto& j : i) {
      acc += j;
    }
  }
  return acc;
}

Grid& Grid::toggle(size_t rowStart,
                   size_t rowEnd,
                   size_t colStart,
                   size_t colEnd) {
  for (size_t i = rowStart; i <= rowEnd; ++i) {
    toggleRow(i, colStart, colEnd);
  }
  return *this;
}

Grid& Grid::setBox(size_t rowStart,
                   size_t rowEnd,
                   size_t colStart,
                   size_t colEnd,
                   arrElem target) {
  for (size_t i = rowStart; i <= rowEnd; ++i) {
    setRow(i, colStart, colEnd, target);
  }
  return *this;
}

Grid& Grid::turnOn(size_t rowStart,
                   size_t rowEnd,
                   size_t colStart,
                   size_t colEnd) {
  for (size_t i = rowStart; i <= rowEnd; ++i) {
    setRow(i, colStart, colEnd, 1);
  }
  return *this;
}

Grid& Grid::turnOff(size_t rowStart,
                    size_t rowEnd,
                    size_t colStart,
                    size_t colEnd) {
  for (size_t i = rowStart; i <= rowEnd; ++i) {
    setRow(i, colStart, colEnd, 0);
  }
  return *this;
}

Grid& Grid::turnUp(size_t rowStart,
                   size_t rowEnd,
                   size_t colStart,
                   size_t colEnd) {
  for (size_t i = rowStart; i <= rowEnd; ++i) {
    for (int j = colStart; j <= colEnd; ++j) {
      ++values[i][j];
    }
  }
  return *this;
}

Grid& Grid::turnDown(size_t rowStart,
                     size_t rowEnd,
                     size_t colStart,
                     size_t colEnd) {
  for (size_t i = rowStart; i <= rowEnd; ++i) {
    for (int j = colStart; j <= colEnd; ++j) {
      arrElem& value = values[i][j];
      if (value != 0) {  // Ensure we do not reach negative brightness.
        --value;
      }
    }
  }
  return *this;
}

Grid& Grid::turnUpUp(size_t rowStart,
                     size_t rowEnd,
                     size_t colStart,
                     size_t colEnd) {
  for (size_t i = rowStart; i <= rowEnd; ++i) {
    for (int j = colStart; j <= colEnd; ++j) {
      ++(++values[i][j]);
    }
  }
  return *this;
}
