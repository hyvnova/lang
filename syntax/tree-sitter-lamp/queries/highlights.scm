(comment) @comment
(multiline_comment) @comment
(string) @string
(number) @number
(boolean) @boolean
(python_content) @embedded

[
  "fn"
  "struct"
  "trait"
  "impl"
  "mod"
  "import"
  "from"
  "use"
  "pub"
  "as"
  "if"
  "elif"
  "else"
  "loop"
  "for"
  "while"
  "in"
] @keyword

(return_statement
  "return" @keyword)

(continue_statement) @keyword
(break_statement) @keyword

[
  "#[python]"
  "#[endpython]"
] @keyword.directive

[
  "->"
  "=>"
  "||"
  "&&"
  "|"
  "&"
  "^"
  "~"
  "!"
  "="
  "+="
  "-="
  "*="
  "/="
  "%="
  "**="
  "=="
  "!="
  "<"
  "<="
  ">"
  ">="
  ".."
  "+"
  "-"
  "*"
  "/"
  "%"
  "**"
  ":="
  "|>"
  "<|"
  "@"
] @operator

[
  "("
  ")"
  "{"
  "}"
  "["
  "]"
] @punctuation.bracket

[
  ","
  "."
  ":"
] @punctuation.delimiter

(attribute_macro
  name: (identifier) @function.macro)

(module_declaration
  name: (identifier) @module)

(import_statement
  alias: (identifier) @module)

(module_path
  head: (identifier) @module)

((identifier) @module.builtin
  (#eq? @module.builtin "std"))

(import_name
  name: (identifier) @variable)

(import_name
  alias: (identifier) @variable)

(function_definition
  name: (identifier) @function)

(method_definition
  name: (identifier) @function.method)

(trait_method_signature
  name: (identifier) @function.method)

(call_expression
  function: (identifier) @function.call)

(call_expression
  function: (member_expression
    property: (identifier) @function.method.call))

(parameter
  name: (identifier) @variable.parameter)

(named_argument
  name: (identifier) @parameter)

(struct_definition
  name: (identifier) @type)

(trait_definition
  name: (identifier) @type)

(impl_block
  trait: (type_ref
    name: (identifier) @type))

(impl_block
  target: (type_ref
    name: (identifier) @type))

(type_ref
  name: (identifier) @type)

(struct_literal
  name: (type_ref
    name: (identifier) @constructor))

(struct_field
  name: (identifier) @property)

(struct_literal_field
  name: (identifier) @property)

(member_expression
  property: (identifier) @property)

(signal_reference
  "$" @operator
  name: (identifier) @variable.special)

(reactive_statement
  "$" @operator)

(binding_definition
  name: (identifier) @variable)

(assignment_statement
  left: (assignment_target
    (identifier) @variable))

(assignment_statement
  left: (assignment_target
    (destructuring_pattern
      name: (identifier) @variable)))

(dictionary_entry
  key: (identifier) @property)
