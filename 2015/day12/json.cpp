#include "json.hpp"
#include <assert.h>
#include <iostream>

// TODO: iostream << operator

namespace json {
// Used to clamp std::string::substring second argument to handle case where
// string has exactly one non-boundary element. Pass in the index in the string
// that you want to evaluate. String is expected to be in the form "___" or
// similar i.e. no leading content
std::string strip_delimiters(std::string s, int i) {
  int j;
  if (i >= 3) { // good case
    j = i - 1;
  } else if (i == 2) { // one element string
    j = 1;
  } else { // empty string
    j = 0;
  }
  return s.substr(1, j);
}

size_t find_bracket(const std::string s) {
  const auto c1 = s.at(0);
  auto c2 = c1;
  switch (c1) {
  case '[':
    c2 = ']';
    break;
  case '{':
    c2 = '}';
    break;
  default:
    std::cerr << "Error in `find_bracket`: first character is " << c1
              << "; string is " << s;
  }
  size_t i = 1;
  size_t level = 0;
  for (; i < s.length(); ++i) {
    const auto c = s.at(i);
    if (s.at(i) == c2 && level == 0) {
      return i;
    }
    switch (c) {
    case '[':
    case '{':
      level++;
      break;
    case ']':
    case '}':
      level--;
      break;
    default:
      continue;
    }
  }
  return i;
}

size_t find_next_element(const std::string s) {
  size_t i = 0;
  size_t level = 0;
  for (; i != s.length() && (s.at(i) != ',' || level != 0); ++i) {
    const auto c = s.at(i);
    if (c == '[' || c == '{') {
      level++;
    } else if (c == ']' || c == '}') {
      level--;
    }
  }
  return i;
}

namespace parse {
value parse_value(const std::string s) {
  auto c = s.at(0);
  size_t i = 0;
  value v;
  if (c == '[') {
    i = find_bracket(s);
    v = parse_array(strip_delimiters(s, i));
  } else if (c == '{') {
    i = find_bracket(s);
    v = parse_object(strip_delimiters(s, i));
  } else if (c == '"') { // Short enough that inline is fine
    i = s.find('"', 1);
    v.type = value::string;
    auto ss = strip_delimiters(s, i);
    v.data.sp = new std::string(ss);
    assert(v.data.sp);
  } else {
    v = parse_other(s);
  }
  return v;
}

value parse_array(std::string s) {
  array_t *a = new array_t;
  assert(a);
  auto &array = *a;
  while (!s.empty()) {
    size_t i = find_next_element(s);
    array.push_back(parse_value(s.substr(0, i)));
    // shorten `s` to contain only remaining elements
    s.erase(0, i);
    if (!s.empty() && s.at(0) == ',') {
      s.erase(0, 1);
    }
  }
  value v;
  v.type = value::array;
  v.data.ap = a;
  return v;
}

value parse_object(std::string s) {
  object_t *o = new object_t;
  assert(o);
  auto &object = *o;
  while (!s.empty()) {
    size_t i = find_next_element(s);
    size_t j = s.find(':');
    // TODO: verify that `key` is a string
    std::string key = strip_delimiters(s, j - 1);
    std::string val = s.substr(j + 1, i - j - 1);
    auto parsed_val = parse_value(val);
    object.insert(std::make_pair(key, parsed_val));
    // shorten `s` to contain only remaining elements
    s.erase(0, i);
    if (!s.empty() && s.at(0) == ',') {
      s.erase(0, 1);
    }
  }
  value v;
  v.type = value::object;
  v.data.op = o;
  return v;
}

// Parses numbers, booleans, null
value parse_other(std::string s) {
  // TODO: verification/cleaning of input string?
  value v;
  auto s0 = s.at(0);
  if (s0 == '-' || ('0' <= s0 && s0 <= '9')) {
    v.type = value::number;
    v.data.i = std::stoi(s);
  } else if (s0 == 'f' || s0 == 't') {
    v.type = value::boolean;
    v.data.b = false;
  } else if (s0 == 'n') {
    v.type = value::null;
  } else {
    std::cerr << "`parse_other` received bad input: " << s;
    exit(1);
  }
  return v;
}

} // namespace parse
} // namespace json
