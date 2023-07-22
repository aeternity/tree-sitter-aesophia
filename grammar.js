const PREC = {
  plus: 15,
  times: 16,
}

module.exports = grammar({
  name: 'aesophia',

  word: $ => $.identifier,

  extras: $ => [
    $.comment,
    /[\s\f\uFEFF\u2060\u200B]|\\\r?\n/
  ],

  externals: $ => [
    $._indent,
    $._dedent,
    $._newline,
  ],

  rules: {
    source_file: $ => repeat($._top_declaration),

    _top_declaration: $ => choice(
      $.contract_declaration
    ),

    contract_declaration: $ => seq(
      optional('payable'),
      'contract',
      $.constructor,
      '=',
      seq(
        $._indent,
        repeat(seq($._declaration, $._newline)),
        $._dedent
      )
    ),

    _declaration: $ => choice(
      $._type_declaration,
      $._record_declaration,
      seq(
        choice(
          seq(
            optional($.entrypoint_modifier),
            'entrypoint'
          ),
          seq(
            optional($.function_modifier),
            'function'
          ),
        ),
        $.function_declaration
      )
    ),

    entrypoint_modifier: $ => choice(
      'payable',
      'stateful'
    ),

    function_modifier: $ => choice(
      'stateful',
      'private'
    ),

    function_declaration: $ => choice(
      // Type signature
      seq(
        $.identifier,
        ':',
        $.type
      ),
      // Definition
      seq(
        $.identifier,
        $.arguments,
        '=',
        choice(
          $.statement,
          seq(
            $._indent,
            repeat(seq($.statement, $._newline)),
            $._dedent
          )
        )
      )
    ),

    arguments: $ => seq(
      '(',
      sep($.pattern, ','),
      ')'
    ),

    // Statements
    statement: $ => choice(
      // seq(
      //   'swtich',
      //   '(',
      //   $.expression,
      //   ')',
      //   block($, $.case)
      // ),
      // seq(
      //   'if',
      //   '(',
      //   $.expression,
      //   ')',
      //   block($, $.statement)
      // ),
      // seq(
      //   'elif',
      //   '(',
      //   $.expression,
      //   ')',
      //   block($, $.statement)
      // ),
      // seq(
      //   'else',
      //   '(',
      //   $.expression,
      //   ')',
      //   block($, $.statement)
      // ),
      // seq(
      //   'let',
      //   $.let_definition
      // ),
      $.expression
    ),

    let_definition: $ => choice(
      seq(
        $.identifier,
        $.arguments,
        optional(seq(
          ':',
          $.type
        )),
        '=',
        block($, $.statement)
      ),
      seq(
        $.pattern,
        '=',
        block($, $.statement)
      )
    ),

    case: $ => seq(
      $.pattern,
      '=>',
      block($, $.statement)
    ),

    pattern: $ => $.expression,

    // Expressions
    expression: $ => choice(
      // Type annotation        5 : int
      seq(
        $.expression,
        ':',
        $.type
      ),
      $.binary_operator,
      // Identifiers
      $.identifier,
      $.constructor,
      $.qualified_identifier,
      // Literals
      $.int
    ),

    binary_operator: $ => {
      const table = [
        [prec.left, '+', PREC.plus],
        [prec.left, '-', PREC.plus],
        [prec.left, '*', PREC.times],
        [prec.left, '/', PREC.times],
      ];

      return choice(...table.map(([fn, operator, precedence]) => fn(precedence, seq(
        field('left', $.expression),
        field('operator', operator),
        field('right', $.expression)
      ))));
    },

    _type_declaration: $ => seq(
      'type',
      $.identifier,
      optional(seq(
        '(',
        optional($.type_variable),
        ')'
      )),
      '=',
      $._type_alias
    ),

    _record_declaration: $ => seq(
      'record',
      $.identifier,
      '=',
      $.record_type
    ),

    _type_alias: $ => $.type,

    record_type: $ => seq(
      '{',
      sep($.field_type, ','),
      '}'
    ),

    field_type: $ => seq(
      $.identifier,
      ':',
      $.type
    ),

    constructor_declaration: $ => seq(
      $.constructor,
      optional(
        seq(
          '(',
          sep1($.type, ','),
          ')'
        )
      )
    ),

    // Types
    type: $ => prec.right(1, choice(
      $.identifier,
      $.qualified_identifier,
      $.type_variable,
      // Function type
      seq($.domain, '=>', $.type),
      // Type application
      seq(
        $.type,
        '(',
        sep($.type, ','),
        ')'
      ),
      // Parens
      seq('(', $.type, ')'),
      // Tuples
      'unit',
      // TODO
      // prec(10, sep1($.type, '*'))
    )),

    domain: $ => choice(
      // Single argument
      $.type,
      // Multiple arguments
      seq(
        '(',
        sep($.type, ','),
        ')'
      )
    ),

    // identifiers start with a lower case letter.
    identifier: $ => /[a-z_][A-Za-z0-9_']*/,

    // constructors start with an upper case letter.
    constructor: $ => /[A-Z][A-Za-z0-9_']*/,

    qualified_identifier: $ => seq(
      repeat1(seq($.constructor, '.')),
      $.identifier
    ),

    type_variable: $ => seq("'", $.identifier),

    int: $ => /[0-9]+(_[0-9]+)*|0x[0-9A-Fa-f]+(_[0-9A-Fa-f]+)*/,

    comment: $ => token(choice(
      seq(
        '//',
        /.*/
      ),
      seq(
        '/*',
        /[^*]*\*+([^/*][^*]*\*+)*/,
        '/'
      )
    )),

    indent: $ => $._indent,
    dedent: $ => $._dedent
  }
});


// function block($, rule) {
//   return rule
// }

function block($, rule) {
  return seq(
    $.indent,
    rule,
    $.dedent
  )
}

    // block: $ => seq(
    //   repeat($._statement),
    //   $._dedent
    // ),

function sep(rule, delimiter) {
  return optional(sep1(rule, delimiter))
}

function sep1(rule, delimiter) {
  return seq(rule, repeat(seq(delimiter, rule)))
}
