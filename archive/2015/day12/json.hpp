#ifndef __JSON__H__
#define __JSON__H__

#include <string>
#include <unordered_map>
#include <vector>

/* Handles limited parsing of JSON files.
Limitations:
    No whitespace, no newline characters.
    Strings cannot contain any of: " [ { } ] ,
    No verification is done.
*/

namespace json {
struct value;
typedef std::vector<value> array_t;
typedef std::unordered_map<std::string, value> object_t;

// `data` pointers must be stack allocated
struct value {
  enum value_t { object, array, string, number, boolean, null } type;
  union json_data {
    int i;
    bool b;
    std::string *sp;
    array_t *ap;
    object_t *op;
  } data;
};

// Returns the index for matching bracket
// First character must be the bracket to match
// Supports [ { "
size_t find_bracket(const std::string s);

/*
Returns the index of the next separating comma, or s.length() if no commas
found.
Expects that the first character is not a comma.
 */
size_t find_next_element(const std::string s);

namespace parse {
// Dispatch function to `parse_array`, `parse_object` or `parse_other`
value parse_value(const std::string);
// Precondition: only contains interior contents of array
value parse_array(std::string);
// Precondition: only contains interior contents of object
value parse_object(std::string);
// Parses numbers, booleans, null
value parse_other(const std::string s);
} // namespace parse

} // namespace json

#endif //__JSON__H__