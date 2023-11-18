/*
 * Copyright (c) 2023, ke0na
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#ifndef JAYSON_JAYSON_H
#define JAYSON_JAYSON_H

#include <cctype>
#include <format>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace Jayson {

enum NodeType { UNKNOWN, VALUE, ELEMENT, MEMBER };

enum ValueType {
  STRING,
  OBJECT,
  ARRAY,
  NUMBER,
  _NULL,
  FALSE,
  TRUE,
};

enum NodeVisitorContinuation {
  CONTINUE,
  BREAK,
};

class Node {
private:
  NodeType m_type = NodeType::UNKNOWN;
  int m_level = 0;

public:
  Node() = default;
  virtual ~Node() = default;
  explicit Node(NodeType type, int level) : m_type{type}, m_level{level} {};
  NodeType get_type() { return m_type; }
  int get_level() { return m_level; }
};

class Value : public Node {
private:
  ValueType m_type;

public:
  Value() = default;
  virtual ~Value() = default;
  explicit Value(ValueType type, int level)
      : Node(NodeType::VALUE, level), m_type{type} {};
  ValueType get_type() const { return m_type; }
  virtual std::string to_string() {
    throw std::invalid_argument("to_string() method on Value not implemented.");
  };
};

template <class T> class OptionalPtr {

private:
  T *m_value_ptr = {nullptr};
  OptionalPtr(T *value) : m_value_ptr{value} {};

public:
  static OptionalPtr<T> create(T *value) { return OptionalPtr<T>(value); };
  static OptionalPtr<T> empty() { return OptionalPtr<T>{nullptr}; };

  bool is_empty() { return m_value_ptr == nullptr; }
  T *get_value() { return m_value_ptr; }
};

class StringValue : public Value {
private:
  std::string m_value;

public:
  StringValue() = default;
  ~StringValue() = default;
  StringValue(int level) : Value(ValueType::STRING, level){};
  StringValue(int level, std::string value)
      : Value(ValueType::STRING, level), m_value(value){};
  void set_value(std::string value) { m_value = value; }
  std::string get_value() const { return m_value; }
  void append_characters(const std::string &string) { m_value.append(string); }
  std::string to_string() {
    return std::string{"StringValue(\"" + m_value + "\")"};
  }

  friend std::ostream &operator<<(std::ostream &stream,
                                  StringValue string_value) {
    stream << string_value.to_string();
    return stream;
  }
};

class Element : public Node {
private:
  Value *m_value;

public:
  Element(int level) : Node(NodeType::ELEMENT, level){};
  Element() = default;
  ~Element() = default;
  Value *get_value() { return m_value; }
  void set_value(Value *value) { m_value = value; }
  std::string to_string() { return m_value->to_string(); }

  friend std::ostream &operator<<(std::ostream &stream, Element element) {
    stream << element.to_string();
    return stream;
  }
};

class NullValue : public Value {
public:
  NullValue(int level) : Value(ValueType::_NULL, level){};
  std::string to_string() { return std::string{"NullValue"}; }

  friend std::ostream &operator<<(std::ostream &stream, NullValue null_value) {
    stream << null_value.to_string();
    return stream;
  }
};

class BooleanValue : public Value {
public:
  BooleanValue(int level, ValueType type) : Value(type, level){};
  std::string to_string() {
    std::string value{};
    if (this->get_type() == ValueType::TRUE)
      value = "TrueValue";
    if (this->get_type() == ValueType::FALSE)
      value = "FalseValue";

    return std::string{"BooleanValue(\"" + value + "\")"};
  }

  friend std::ostream &operator<<(std::ostream &stream,
                                  BooleanValue bool_value) {
    stream << bool_value.to_string();
    return stream;
  }
};

class NumberValue : public Value {
private:
  std::string m_value;

public:
  NumberValue(std::string &value, int level)
      : Value(ValueType::NUMBER, level), m_value{value} {};
  ~NumberValue() = default;
  std::string get_value() const { return m_value; };

  std::string to_string() {
    return std::string{"NumberValue(\"" + m_value + "\")"};
  }

  friend std::ostream &operator<<(std::ostream &stream,
                                  NumberValue number_value) {
    stream << number_value.to_string();
    return stream;
  }
};

class Member : public Node {
private:
  StringValue *m_string;
  Element *m_element;

public:
  Member(StringValue *string, Element *element, int level)
      : Node(NodeType::MEMBER, level), m_element{element}, m_string{string} {}
  StringValue *get_string() { return m_string; }
  Element *get_element() { return m_element; }
  std::string to_string() {
    std::string stream{};
    for (int index = 0; index < this->get_level(); index++)
      stream += "  ";
    stream += "\"" + this->get_string()->get_value() + "\"" + ": " +
              this->get_element()->get_value()->to_string();
    if (m_element->get_value()->get_type() != ValueType::OBJECT)
      stream += '\n';
    return stream;
  }

  friend std::ostream &operator<<(std::ostream &stream, Member member) {
    stream << member.to_string();
    return stream;
  }
};

class ObjectValue : public Value {
private:
  std::vector<Member *> m_members{};

public:
  ObjectValue(int level) : Value(ValueType::OBJECT, level){};
  ObjectValue() = default;
  ~ObjectValue() = default;
  void add_member(Member *member) { m_members.push_back(member); }
  std::vector<Member *> get_members() { return m_members; }

  std::string to_string() {
    std::string stream{};
    stream += get_level();
    stream += "ObjectValue";
    stream += "\n";
    auto members = this->get_members();

    if (members.empty())
      stream += "\"{}\"";
    else {
      for (auto member : members)
        stream += member->to_string();
    }

    return stream;
  }

  friend std::ostream &operator<<(std::ostream &stream,
                                  ObjectValue object_value) {

    stream << object_value.to_string();
    return stream;
  }
};

class ArrayValue : public Value {
private:
  std::vector<Element *> m_elements{};

public:
  ArrayValue() = default;
  ArrayValue(int level) : Value(ValueType::ARRAY, level){};
  ~ArrayValue() = default;
  void add_member(Element *element) { m_elements.push_back(element); }
  std::vector<Element *> get_elements() { return m_elements; }

  std::string to_string() {
    std::stringstream stream{};
    stream << "ArraValue" << std::endl;
    auto elements = this->get_elements();
    for (auto element : elements) {
      for (int indent = 0; indent < this->get_level(); indent++)
        stream << "   ";
      stream << element->get_value()->to_string() << std::endl;
    }

    return stream.str();
  }

  friend std::ostream &operator<<(std::ostream &stream,
                                  ArrayValue array_value) {
    stream << array_value.to_string();
    return stream;
  }
};

class Json {
private:
  Element *m_element{};

  OptionalPtr<Value> find_element_by_key(const std::string &key) const {
    Value *value = nullptr;

    walk_elements([&value, &key](Node *node) {
      auto node_type = node->get_type();

      if (node_type == NodeType::MEMBER) {
        auto member = dynamic_cast<Member *>(node);
        auto member_key = member->get_string();
        auto member_key_value = member_key->get_value();
        if (!key.compare(member_key_value)) {
          value = member->get_element()->get_value();
          return NodeVisitorContinuation::BREAK;
        }
      };
      return NodeVisitorContinuation::CONTINUE;
    });

    return OptionalPtr<Value>::create(value);
  }

  template <typename T> OptionalPtr<T> find_and_cast(const std::string &key) {
    auto optional_value = find_element_by_key(key);

    if (optional_value.is_empty())
      return OptionalPtr<T>::empty();

    auto value = dynamic_cast<T *>(optional_value.get_value());
    return OptionalPtr<T>::create(value);
  }

public:
  Json() = default;
  explicit Json(Element *element) : m_element(element){};
  [[nodiscard]] Element *get_element() const { return m_element; }

  static void
  visit_element(Node *node,
                std::function<NodeVisitorContinuation(Node *)> const &visitor) {

    auto type = node->get_type();

    if (type == NodeType::VALUE) {

      auto value_node = dynamic_cast<Value *>(node);
      auto value_type = value_node->get_type();

      if (value_type == ValueType::ARRAY) {
        auto array = dynamic_cast<ArrayValue *>(node);
        auto continuation = visitor(array);
        if (continuation == NodeVisitorContinuation::BREAK)
          return;

        auto elements = array->get_elements();
        for (auto element : elements) {
          auto continuation = visitor(element);
          if (continuation == NodeVisitorContinuation::BREAK)
            return;

          visit_element(element->get_value(), visitor);
        }
      } else if (value_type == ValueType::OBJECT) {
        auto object = dynamic_cast<ObjectValue *>(node);
        auto continuation = visitor(object);
        if (continuation == NodeVisitorContinuation::BREAK)
          return;

        auto members = object->get_members();
        for (auto member : members)
          visit_element(member, visitor);
      } else {

        visitor(value_node);
      }
    } else if (type == NodeType::ELEMENT) {
      auto element = dynamic_cast<Element *>(node);
      auto continuation = visitor(element);
      if (continuation == NodeVisitorContinuation::BREAK)
        return;

      visit_element(element->get_value(), visitor);
    } else if (type == NodeType::MEMBER) {
      auto member = dynamic_cast<Member *>(node);
      auto continuation = visitor(member);
      if (continuation == NodeVisitorContinuation::BREAK)
        return;

      visit_element(member->get_element(), visitor);
    }
  }

  void walk_elements(
      std::function<NodeVisitorContinuation(Node *)> const &visitor) const {
    visit_element(this->get_element()->get_value(), visitor);
  }

  OptionalPtr<StringValue> find_string(const std::string &key) {
    return find_and_cast<StringValue>(key);
  }

  OptionalPtr<BooleanValue> find_bool(const std::string &key) {
    return find_and_cast<BooleanValue>(key);
  }

  OptionalPtr<NullValue> find_null(const std::string &key) {
    return find_and_cast<NullValue>(key);
  }

  OptionalPtr<NumberValue> find_number(const std::string &key) {
    return find_and_cast<NumberValue>(key);
  }

  OptionalPtr<ObjectValue> find_object(const std::string &key) {
    return find_and_cast<ObjectValue>(key);
  }

  OptionalPtr<ArrayValue> find_array(const std::string &key) {
    return find_and_cast<ArrayValue>(key);
  }
};

inline std::ostream &operator<<(std::ostream &stream, const Json *json) {

  auto element = json->get_element();
  auto value = element->get_value();
  auto value_type = value->get_type();

  if (value_type == ValueType::ARRAY) {
    auto array = dynamic_cast<ArrayValue *>(value);
    stream << *array;
  }

  if (value_type == ValueType::OBJECT) {
    auto object = dynamic_cast<ObjectValue *>(value);
    stream << *object;
  }

  return stream;
}

class Parser {

private:
  int m_index = 0;
  int m_level = 0;
  std::basic_string<char32_t> m_json_code;

  int m_max_depth_level = 2000;

  void PANIC_IF(bool expression, const std::string &message) const {
    if (expression)
      throw std::invalid_argument(message +
                                  " at position: " + std::to_string(m_index));
  }
  void PANIC(const std::string &message) const { PANIC_IF(true, message); }

  void VERIFIY_MAX_DEPTH_REACHED() const {
    PANIC_IF(m_level >= m_max_depth_level, "Max depth of json reached.");
  }

  void level_up() { m_level++; };
  void level_down() { m_level--; };
  void go_forward() { m_index++; };

  [[nodiscard]] bool is_eof() { return m_json_code[m_index] == '\0'; }
  [[nodiscard]] bool is_not_eof() { return !is_eof(); }
  [[nodiscard]] char32_t pick() const { return m_json_code[m_index]; }
  [[nodiscard]] char32_t consume() { return m_json_code[m_index++]; }

  [[nodiscard]] bool try_consume(const char32_t expected) {
    auto current = pick();
    if (current != expected)
      return false;

    go_forward();
    return true;
  }

  [[nodiscard]] bool
  try_consume_multiple(const std::string &multiple_characters) {
    return std::all_of(multiple_characters.begin(), multiple_characters.end(),
                       [this](char expected) { return expected == consume(); });
  }

  [[nodiscard]] char32_t consume_specific(char excpected) {
    auto character = consume();
    PANIC_IF(character != excpected,
             "not expected character " + std::to_string(excpected));
    return character;
  }

  [[nodiscard]] char32_t try_consume_specific(char excpected) {
    auto character = pick();
    if (character != excpected)
      return {};
    go_forward();
    return character;
  }

  void parse_ws() {
    auto character = pick();

    auto SPACE = 0x20;
    auto HORIZONTAL_TAB = 0x09;
    auto LINE_FEED_NEW_LINE = 0x0A;
    auto CARRIAGE_RETURN = 0x0D;

    while ((character == SPACE || character == HORIZONTAL_TAB ||
            character == LINE_FEED_NEW_LINE || character == CARRIAGE_RETURN) &&
           m_index <= m_json_code.length()) {
      go_forward();
      character = pick();
    }
  }

  bool is_unescaped_char(const int character) {

    return (character == ' ' || character == '!') ||
           (character >= 35 && character <= 91) ||
           (character >= 93 && character <= 127);
  }

  bool is_escape_char(const char32_t character) { return character == '\\'; }

  std::string parse_escape_char() {

    PANIC_IF(!consume_specific('\\'), "Character '\\' exptected");

    auto character = consume();

    if (character == '"' || character == '\\' || character == '/' ||
        character == 'b' || character == 'f' || character == 'n' ||
        character == 'r' || character == 't') {

      return std::string{"\\" + std::to_string(character)};
    } else if (character == 'u') {
      int boundary_index = 0;
      std::string result{"\\u"};

      do {
        auto actual_character = consume();

        // NOTE: could also be parsed without using std:: implementation
        PANIC_IF(!std::isxdigit(static_cast<int>(actual_character)),
                 "unicode has to be hex hex hex hex");
        result += std::to_string(actual_character);

        boundary_index++;
      } while (boundary_index < 4);

      return result;
    }

    return {};
  }

  std::string parse_character() {

    if (is_escape_char(pick()))
      return parse_escape_char();

    auto character = pick();

    if (character == '"')
      return {};

    // ascii
    if (character <= 127)
      return std::string{(char)consume()};

    // overkill but it works
    std::stringstream stream;
    stream << "\\U+" << std::hex << (char)consume();

    // unicode - we allow all control characters as well
    return stream.str();
  }

  StringValue *parse_string() {

    if (consume_specific('\"')) {
      auto string = new StringValue(m_level);

      if (try_consume_specific('"'))
        return string;

      std::string parsed_string;
      do {
        parsed_string = parse_character();
        string->append_characters(parsed_string);
      } while (!parsed_string.empty() && !is_eof());

      PANIC_IF(!try_consume_specific('"'),
               "String should be closed with a \".");

      return string;
    }

    PANIC("Not a valid string");
    return {};
  }

  void parse_member(ObjectValue *object) {

    parse_ws();

    PANIC_IF(pick() != '"', "Member definition not correct");

    auto string = parse_string();

    parse_ws();
    if (consume_specific(':')) {
      auto element = parse_element();
      auto member = new Member(string, element, m_level);
      object->add_member(member);
      return;
    }
  }

  void parse_members(ObjectValue *object) {

    do {
      parse_member(object);
    } while (try_consume_specific(','));
  }

  ObjectValue *parse_object() {

    parse_ws();

    if (consume_specific('{')) {

      VERIFIY_MAX_DEPTH_REACHED();
      level_up();

      auto object = new ObjectValue(m_level);

      if (try_consume_specific('}')) {
        level_down();
        return object;
      }

      parse_members(object);
      parse_ws();

      PANIC_IF(consume() != '}', "Object should be closed with a closing '}'.");

      level_down();
      return object;
    }

    return {};
  }

  int parse_digit() {
    int character_as_int = int(pick() - '0');

    if (character_as_int >= 0 && character_as_int <= 9) {
      go_forward();
      return character_as_int;
    }

    return -1;
  }

  std::string parse_digits() {
    std::string result;
    int character;

    do {
      character = parse_digit();

      if (try_consume_specific('\n') || try_consume_specific('\t'))
        continue;

      if (character < 0)
        return result;

      result.append(std::to_string(character));
    } while (!is_eof());

    return result;
  }

  std::string parse_integer() {
    std::string sign;
    auto character = pick();

    if (character == '-') {
      sign = "-";
      go_forward();
    }

    auto character_as_int = int(consume() - '0');

    if (character_as_int == 0) {
      auto number = std::to_string(0);
      if (!sign.empty())
        return sign + number;

      return number;
    }

    if (character_as_int > 0 && character_as_int <= 9) {
      auto number = std::to_string(character_as_int) + parse_digits();
      if (!sign.empty())
        return sign + number;

      return number;
    }

    PANIC("Integer could not be parsed.");

    return {};
  }

  std::string parse_fraction() { return '.' + parse_digits(); }

  std::string parse_exponent() {

    PANIC_IF(!is_digit(pick()), "Should follow a number ");

    std::string result;
    result.push_back('E');
    result.push_back((char)consume());
    result += parse_digits();

    return result;
  }

  NumberValue *parse_number() {

    std::string number{parse_integer()};

    if (pick() == '.') {
      go_forward();
      PANIC_IF(!could_be_number(pick()),
               "A number should follow after the '.' ");

      number += parse_fraction();
    }

    if (pick() == 'E' || pick() == 'e') {
      go_forward();
      if (pick() == '-' || pick() == '+')
        number += (char)consume();

      number += parse_exponent();
    }

    return new NumberValue(number, m_level);
  }

  bool is_digit(const char32_t character) {
    return character >= 48 && character <= 57;
  }

  bool could_be_number(const char32_t character) {

    auto is_digit_zero_to_nine = is_digit(character);
    auto is_starting_with_dash = character == '-';

    return is_digit_zero_to_nine || is_starting_with_dash;
  }

  Value *parse_null() {
    PANIC_IF(!try_consume_multiple("null"), "null value is invalid");
    return new NullValue(m_level);
  }

  Value *parse_false() {
    PANIC_IF(!try_consume_multiple("false"), "false value is invalid");
    return new BooleanValue(m_level, ValueType::FALSE);
  }

  Value *parse_true() {
    PANIC_IF(!try_consume_multiple("true"), "true value is invalid");
    return new BooleanValue(m_level, ValueType::TRUE);
  }

  Value *parse_value() {

    if (pick() == '{')
      return parse_object();

    if (pick() == '\"')
      return parse_string();

    if (pick() == '[')
      return parse_array();

    if (pick() == 'n')
      return parse_null();

    if (pick() == 't')
      return parse_true();

    if (pick() == 'f')
      return parse_false();

    if (could_be_number(pick()))
      return parse_number();
    PANIC("invalid character when parsing value");
    return {};
  }

  Element *parse_element() {
    parse_ws();

    auto element = new Element(m_level);
    auto parsed_value = parse_value();
    element->set_value(parsed_value);

    parse_ws();
    return element;
  }

  void parse_elements(ArrayValue *array) {

    do {
      array->add_member(parse_element());
    } while (try_consume_specific(','));
  }

  ArrayValue *parse_array() {

    parse_ws();

    if (consume_specific('[')) {

      if (try_consume_specific(']'))
        return new ArrayValue(m_level);

      VERIFIY_MAX_DEPTH_REACHED();
      level_up();

      auto array = new ArrayValue(m_level);
      parse_elements(array);

      PANIC_IF(consume() != ']', "Array should be closed with a closing ']'.");

      level_down();

      return array;
    }

    return {};
  }

public:
  explicit Parser(std::basic_string<char32_t> json_code)
      : m_json_code{std::move(json_code)} {};

  Json *parse() {
    auto json = new Json(parse_element());
    PANIC_IF(is_not_eof(),
             "Please remove this character for closing the json.");
    return json;
  }
};
} // namespace Jayson

#endif // JAYSON_JAYSON_H
