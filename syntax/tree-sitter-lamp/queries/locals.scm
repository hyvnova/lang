(block) @local.scope
(function_definition
  body: (block) @local.scope)
(method_definition
  body: (block) @local.scope)
(lambda_expression
  parameters: (parameter_list) @local.scope)

(parameter
  name: (identifier) @local.definition)

(function_definition
  name: (identifier) @local.definition)

(method_definition
  name: (identifier) @local.definition)

(assignment_statement
  left: (assignment_target
    (identifier) @local.definition))

(assignment_statement
  left: (assignment_target
    (destructuring_pattern
      name: (identifier) @local.definition)))

(signal_assignment
  target: (signal_reference
    name: (identifier) @local.definition))

(identifier) @local.reference
