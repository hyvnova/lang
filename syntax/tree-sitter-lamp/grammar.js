const PREC = {
  assignment: 1,
  distribution: 2,
  pipe: 3,
  logical_or: 4,
  logical_and: 5,
  bitwise_or: 6,
  bitwise_xor: 7,
  bitwise_and: 8,
  equality: 9,
  comparison: 10,
  range: 11,
  additive: 12,
  multiplicative: 13,
  power: 14,
  unary: 15,
  call: 16,
  member: 17,
};

module.exports = grammar({
  name: "lamp",

  externals: ($) => [$._python_content],

  extras: ($) => [/\s/, ";", $.comment, $.multiline_comment],

  word: ($) => $.identifier,

  supertypes: ($) => [$._statement, $._expression],

  conflicts: ($) => [
    [$.assignment_statement, $.binding_definition],
    [$.struct_literal, $.block],
    [$.parameter_list, $.parenthesized_expression],
    [$.module_path, $.member_expression],
    [$.call_expression, $.lambda_expression],
    [$.type_ref, $._expression],
    [$.parameter, $._expression],
    [$.distribution_expression],
  ],

  rules: {
    source_file: ($) => repeat($._statement),

    _statement: ($) =>
      choice(
        $.attribute_item,
        $.python_block,
        $.module_declaration,
        $.import_statement,
        $.from_import_statement,
        $.use_declaration,
        $.function_definition,
        $.struct_definition,
        $.trait_definition,
        $.impl_block,
        $.conditional_statement,
        $.loop_statement,
        $.while_statement,
        $.for_statement,
        $.return_statement,
        $.continue_statement,
        $.break_statement,
        $.reactive_statement,
        $.signal_assignment,
        $.binding_definition,
        $.assignment_statement,
        $.expression_statement,
      ),

    comment: ($) => token(seq("//", /.*/)),

    multiline_comment: ($) =>
      token(seq("/*", repeat(choice(/[^*]/, /\*[^/]/)), /\*+\//)),

    attribute_item: ($) =>
      seq(repeat1($.attribute_macro), choice($.struct_definition, $.trait_definition)),

    attribute_macro: ($) =>
      seq("#", field("name", $.identifier), field("arguments", $.argument_list)),

    python_block: ($) =>
      seq(
        "#[python]",
        optional(field("content", alias($._python_content, $.python_content))),
        "#[endpython]",
      ),

    module_declaration: ($) =>
      seq(optional("pub"), "mod", field("name", $.identifier)),

    import_statement: ($) =>
      seq(
        "import",
        field("path", $.module_path),
        optional(seq("as", field("alias", $.identifier))),
      ),

    from_import_statement: ($) =>
      seq(
        "from",
        field("path", $.module_path),
        "import",
        choice("*", commaSep1($.import_name)),
      ),

    import_name: ($) =>
      seq(field("name", $.identifier), optional(seq("as", field("alias", $.identifier)))),

    use_declaration: ($) =>
      seq(
        optional("pub"),
        "use",
        field("path", $.module_path),
        optional(seq("as", field("alias", $.identifier))),
      ),

    function_definition: ($) =>
      seq(
        optional("pub"),
        "fn",
        field("name", $.identifier),
        field("parameters", $.parameter_list),
        optional($.return_type),
        field("body", $.block),
      ),

    struct_definition: ($) =>
      seq(
        optional("pub"),
        "struct",
        field("name", $.identifier),
        optional(field("type_parameters", $.type_parameters)),
        field("body", $.struct_body),
      ),

    struct_body: ($) => seq("{", optional(commaSep1($.struct_field)), optional(","), "}"),

    struct_field: ($) =>
      seq(field("name", $.identifier), ":", field("type", $.type_ref)),

    trait_definition: ($) =>
      seq(
        optional("pub"),
        "trait",
        field("name", $.identifier),
        optional(field("type_parameters", $.type_parameters)),
        field("body", $.trait_body),
      ),

    trait_body: ($) => seq("{", repeat($.trait_method_signature), "}"),

    trait_method_signature: ($) =>
      seq(
        "fn",
        field("name", $.identifier),
        field("parameters", $.parameter_list),
        optional($.return_type),
      ),

    impl_block: ($) =>
      seq(
        "impl",
        optional(field("type_parameters", $.type_parameters)),
        choice(
          seq(field("trait", $.type_ref), "for", field("target", $.type_ref)),
          field("target", $.type_ref),
        ),
        field("body", $.impl_body),
      ),

    impl_body: ($) => seq("{", repeat($.method_definition), "}"),

    method_definition: ($) =>
      seq(
        "fn",
        field("name", $.identifier),
        field("parameters", $.parameter_list),
        optional($.return_type),
        field("body", $.block),
      ),

    binding_definition: ($) =>
      seq("pub", field("name", $.identifier), "=", field("value", $._expression)),

    assignment_statement: ($) =>
      prec.right(
        PREC.assignment,
        seq(
          field("left", commaSep1($.assignment_target)),
          field("operator", $.assignment_operator),
          field("right", commaSep1($._expression)),
        ),
      ),

    signal_assignment: ($) =>
      prec.right(
        PREC.assignment,
        seq(
          field("target", $.signal_reference),
          field("operator", $.assignment_operator),
          field("value", $._expression),
        ),
      ),

    assignment_target: ($) =>
      choice($.identifier, $.member_expression, $.index_expression, $.signal_reference, $.destructuring_pattern),

    destructuring_pattern: ($) =>
      seq("{", commaSep1(field("name", $.identifier)), optional(","), "}"),

    conditional_statement: ($) =>
      seq(
        "if",
        field("condition", $._expression),
        field("consequence", $.block),
        repeat($.elif_clause),
        optional($.else_clause),
      ),

    elif_clause: ($) =>
      seq("elif", field("condition", $._expression), field("consequence", $.block)),

    else_clause: ($) => seq("else", field("consequence", $.block)),

    loop_statement: ($) => seq("loop", field("body", $.block)),

    while_statement: ($) =>
      seq("while", field("condition", $._expression), field("body", $.block)),

    for_statement: ($) =>
      seq(
        "for",
        field("item", choice($.identifier, $.destructuring_pattern)),
        "in",
        field("iterable", $._expression),
        field("body", $.block),
      ),

    return_statement: ($) => prec.right(seq("return", optional($._expression))),

    continue_statement: () => "continue",

    break_statement: () => "break",

    reactive_statement: ($) => seq("$", field("body", $.block)),

    expression_statement: ($) => prec(-1, $._expression),

    block: ($) => seq("{", repeat($._statement), optional($._expression), "}"),

    module_path: ($) =>
      prec.right(seq(repeat("."), field("head", $.identifier), repeat(seq(".", $.identifier)))),

    parameter_list: ($) => seq("(", optional(commaSep1($.parameter)), optional(","), ")"),

    parameter: ($) =>
      seq(field("name", $.identifier), optional(seq(":", field("type", $.type_ref)))),

    return_type: ($) => seq("->", $.type_ref),

    type_parameters: ($) => seq("<", commaSep1($.identifier), optional(","), ">"),

    type_arguments: ($) => seq("<", commaSep1($.type_ref), optional(","), ">"),

    type_ref: ($) =>
      prec.right(
        seq(field("name", $.identifier), optional(field("type_arguments", $.type_arguments))),
      ),

    argument_list: ($) => seq("(", optional(commaSep1($.argument)), optional(","), ")"),

    argument: ($) => choice($.named_argument, $._expression),

    named_argument: ($) =>
      seq(field("name", $.identifier), "=", field("value", $._expression)),

    _expression: ($) =>
      choice(
        $.lambda_expression,
        $.distribution_expression,
        $.binary_expression,
        $.unary_expression,
        $.call_expression,
        $.member_expression,
        $.index_expression,
        $.struct_literal,
        $.parenthesized_expression,
        $.array_expression,
        $.dictionary_expression,
        $.signal_reference,
        $.identifier,
        $.number,
        $.string,
        $.boolean,
      ),

    lambda_expression: ($) =>
      prec.right(seq(field("parameters", $.parameter_list), "=>", field("body", choice($._expression, $.block)))),

    distribution_expression: ($) =>
      prec.right(
        PREC.distribution,
        choice(
          seq(field("value", $._expression), "->", field("recipient", $._expression)),
          seq("|>", field("value", $._expression), "->", field("recipient", $._expression)),
          seq(field("recipient", $._expression), "<|", field("value", $._expression)),
        ),
      ),

    binary_expression: ($) =>
      choice(
        ...[
          ["||", PREC.logical_or],
          ["&&", PREC.logical_and],
          ["|", PREC.bitwise_or],
          ["^", PREC.bitwise_xor],
          ["&", PREC.bitwise_and],
          ["==", PREC.equality],
          ["!=", PREC.equality],
          ["<", PREC.comparison],
          ["<=", PREC.comparison],
          [">", PREC.comparison],
          [">=", PREC.comparison],
          ["..", PREC.range],
          ["+", PREC.additive],
          ["-", PREC.additive],
          ["*", PREC.multiplicative],
          ["/", PREC.multiplicative],
          ["%", PREC.multiplicative],
          ["**", PREC.power],
          [":=", PREC.assignment],
        ].map(([operator, precedence]) =>
          prec.left(
            precedence,
            seq(field("left", $._expression), field("operator", operator), field("right", $._expression)),
          ),
        ),
      ),

    unary_expression: ($) =>
      prec.left(
        PREC.unary,
        seq(field("operator", choice("!", "-", "+", "~", "*", "&", "@")), field("argument", $._expression)),
      ),

    call_expression: ($) =>
      prec(
        PREC.call,
        seq(field("function", choice($.identifier, $.member_expression, $.index_expression, $.parenthesized_expression, $.lambda_expression, $.call_expression, $.signal_reference)), field("arguments", $.argument_list)),
      ),

    member_expression: ($) =>
      prec(
        PREC.member,
        seq(field("object", choice($.identifier, $.call_expression, $.index_expression, $.parenthesized_expression, $.member_expression, $.signal_reference, $.struct_literal)), ".", field("property", $.identifier)),
      ),

    index_expression: ($) =>
      prec(
        PREC.member,
        seq(field("object", choice($.identifier, $.call_expression, $.member_expression, $.parenthesized_expression, $.index_expression, $.signal_reference, $.array_expression, $.struct_literal)), "[", field("index", $._expression), "]"),
      ),

    struct_literal: ($) =>
      prec(PREC.call, seq(field("name", $.type_ref), field("body", $.struct_literal_body))),

    struct_literal_body: ($) =>
      seq("{", optional(commaSep1($.struct_literal_field)), optional(","), "}"),

    struct_literal_field: ($) =>
      seq(field("name", $.identifier), ":", field("value", $._expression)),

    array_expression: ($) => seq("[", optional(commaSep1($._expression)), optional(","), "]"),

    dictionary_expression: ($) =>
      seq("{", commaSep1($.dictionary_entry), optional(","), "}"),

    dictionary_entry: ($) =>
      seq(field("key", $._expression), ":", field("value", $._expression)),

    parenthesized_expression: ($) =>
      seq("(", optional(commaSep1($._expression)), optional(","), ")"),

    signal_reference: ($) => seq("$", field("name", $.identifier)),

    boolean: () => choice("true", "false"),

    number: () => /\d+(\.\d+)?/,

    string: () =>
      choice(
        seq('"', repeat(choice(token.immediate(/[^"\\\n]+/), token.immediate(/\\./))), '"'),
        seq("'", repeat(choice(token.immediate(/[^'\\\n]+/), token.immediate(/\\./))), "'"),
      ),

    identifier: () => /[A-Za-z_][A-Za-z0-9_]*/,

    assignment_operator: () => choice("=", "+=", "-=", "*=", "/=", "**=", "%="),
  },
});

function commaSep1(rule) {
  return seq(rule, repeat(seq(",", rule)));
}
