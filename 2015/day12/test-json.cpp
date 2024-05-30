// ad-hoc test file for debugging

#include "json.cpp"
#include <assert.h>
#include <iostream>

int test_array() {
  std::cout << "test_array()" << std::endl;
  std::string s = "1,2,3";
  std::cout << "Input: \"" << s << '"' << std::endl;
  auto x = json::parse::parse_array(s);
  for (auto &x : *(x.data.ap)) {
    std::cout << x.data.i << " ";
  }
  std::cout << '\n';
  return 0;
}

int test_value_array() {
  std::cout << "test_value_array()" << std::endl;
  std::string s = "[1,2,3]";
  std::cout << "Input: \"" << s << '"' << std::endl;
  auto x = json::parse::parse_value(s);
  for (auto &x : *(x.data.ap)) {
    std::cout << x.data.i << " ";
  }
  std::cout << '\n';
  return 0;
}

int test_value_string() {
  std::cout << "test_value_string()" << std::endl;
  std::string s = "\"a\"";
  std::cout << "Input: \"" << s << '"' << std::endl;
  auto x = json::parse::parse_value(s);
  std::cout << *x.data.sp << std::endl;
  return 0;
}

int test_object() {
  std::cout << "test_object()" << std::endl;
  std::string s = "\"a\":1,\"b\":\"b\"";
  std::cout << "Input: \"" << s << '"' << std::endl;
  auto x = json::parse::parse_object(s);
  assert(x.type == json::value::object);
  for (auto &y : *x.data.op) {
    std::cout << y.first << " : ";
    if (y.second.type == json::value::string) {
      std::cout << *y.second.data.sp;
    } else if (y.second.type == json::value::number) {
      std::cout << y.second.data.i;
    } else {
      std::cout << "ERROR";
      exit(1);
    }
    std::cout << ", ";
  }
  return 0;
}

int main() {
  test_value_string();
  test_array();
  test_value_array();
  test_object();
  return 0;
}
