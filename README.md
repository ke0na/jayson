# Jayson
Header-only JSON parser. Simple to include JSON parser for your small cpp application.

# Testing
Done with the help of: https://github.com/nst/JSONTestSuite

# Build & run example
```
cd example
mkdir build
cd build
cmake -GNinja ..
ninja
./jayson ../test.json 
```

# Api Jayson::Parser
First init the parser by giving the input as a `std::string`.

If an error occurs during the parsing an exception ges thrown.
```
// init parser
Jayson::Parser parser{input_file_string};

// parse
Jayson::Json json = parser.parse();
```

# Api Jayson::Json

## Search
Find the first node in the json tree which matches the given key. Return value is 
an `Jayson::OptionalPtr` which contains either the value or not.

```
json->find_string(key); 
json->find_number(key); 
json->find_bool(key); 
json->find_object(key); 
json->find_array(key); 
```

## Visit nodes
Visiting all the nodes in the json tree. The recursive calls can either be `continued` or not (`break`). 
See an example in the implementation header file for searching the `node` by a given key.

```
json->walk_elements(std::function<NodeVisitorContinuation(Node *)> const &visitor);
```

# Spec
- RFC8259 https://www.rfc-editor.org/rfc/rfc8259
- https://www.json.org/json-en.html
```
json
    element

value
    object
    array
    string
    number
    "true"
    "false"
    "null"

object
    '{' ws '}'
    '{' members '}'

members
    member
    member ',' members

member
    ws string ws ':' element

array
    '[' ws ']'
    '[' elements ']'

elements
    element
    element ',' elements

element
    ws value ws

string
    '"' characters '"'

characters
    ""
    character characters

character
    '0020' . '10FFFF' - '"' - '\'
    '\' escape

escape
    '"'
    '\'
    '/'
    'b'
    'f'
    'n'
    'r'
    't'
    'u' hex hex hex hex

hex
    digit
    'A' . 'F'
    'a' . 'f'

number
    integer fraction exponent

integer
    digit
    onenine digits
    '-' digit
    '-' onenine digits

digits
    digit
    digit digits

digit
    '0'
    onenine

onenine
    '1' . '9'

fraction
    ""
    '.' digits

exponent
    ""
    'E' sign digits
    'e' sign digits

sign
    ""
    '+'
    '-'

ws
    ""
    '0020' ws
    '000A' ws
    '000D' ws
    '0009' ws
```
