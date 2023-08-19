const PREC = {
  plus: 15,
  times: 16,
}

module.exports = grammar({
  name: 'aesophia',

  conflicts: $ => [
    [$.list_literal, $.map_access]
  ],

  extras: $ => [
    $.comment,
    /[\s\f\uFEFF\u2060\u200B]|\\\r?\n/
  ],

  // externals: $ => [
  //   $._indent,
  //   $._dedent,
  //   $._newline,
  // ],

  rules: {
    source_file: $ => seq(
      repeat($._top_pragma),
      repeat($.include),
      repeat($.using),
      repeat($._top_declaration)
    ),

    _top_pragma: $ => 'TODO',

    _top_declaration: $ => choice(
      $.contract_declaration
    ),

    using: $ => seq(
      'using',
      $._scope,
      optional(choice(seq('hiding', $._name), seq('for', $._name)))
    ),

    include: $ => seq('include', 'TODO'),

    contract_declaration: $ => seq(
      optional('payable'),
      'contract',
      $._scope_name,
      '=',
      maybe_block($, $._scoped_declaration)
      // $.function_declaration,
    ),

    _scoped_declaration: $ => choice(
      $.type_declaration,
      $.function_declaration,
    ),

    function_declaration: $ => seq(
      repeat($.function_modifier),
      $.function_head,
      maybe_block($, choice(
        $.function_signature,
        $.function_clause
      ))
    ),

    function_signature: $ => seq(
      $._function_name,
      ':',
      $.type
    ),

    function_clause: $ => seq(
      $._function_name,
      '(', sep($._pattern, ','), ')',
      optional(seq(':', $.type)),
      '=',
      $.expression
    ),

    function_head: $ => choice(
      'function',
      'entrypoint'
    ),

    function_modifier: $ => choice(
      'payable',
      'stateful',
      'private'
    ),

    type_declaration: $ => seq(
      'type',
      $._type_name,
      optional(seq(
        '(',
        sep1($._type_variable_name, ','),
        ')'
      )),
      '=',
      choice(
        $.type_alias,
        $.record_declaration,
        $.variant_declaration
      )
    ),

    type_alias: $ => $.type,

    record_declaration: $ => seq(
      '{',
      sep($.field_declaration, ','),
      '}'
    ),

    field_declaration: $ => seq(
      $._field_name,
      ':',
      $.type
    ),

    variant_declaration: $ => seq(
      sep1($.constructor_declaration, '|')
    ),

    constructor_declaration: $ => seq(
      $._constructor_name,
      optional(
        seq(
          '(',
          sep1($.type, ','),
          ')'
        )
      )
    ),

    expression: $ => choice(
      $.expr_lambda,
      $.expr_typed,
      $.expr_op,
      $.expr_application,
      $.expr_record_update,
      $.expr_map_update,
      $.expr_projection,
      $.expr_switch,
      $.expr_block,
      $._expr_atom
    ),

    expr_lambda: $ => seq(
      '(', sep($._pattern, ','),
      ')', '=>',
      $.expression
    ),

    expr_typed: $ => prec.left(100, seq(
      $.expression, ':', $.type
    )),

    expr_op: $ => prec(200, choice(
      prec.left(200, _expr_op($, '|>')
               ),
      prec.right(210,
                 _expr_op($, '||')
                ),
      prec.right(220,
                 _expr_op($, '&&')
                ),
      prec.left(230,
           _expr_op($, choice('<', '>', '=<', '>=', '==', '!='))
          ),
      prec.right(240,
                 _expr_op($, choice('::', '++'))
                ),
      prec.left(250,
                _expr_op($, choice('+', '-'))
               ),
      prec(260,
           seq(repeat1('-'), $.expression)
          ),
      prec.left(270,
                _expr_op($, choice('*', '/', 'mod'))
               ),
      prec.left(280,
                _expr_op($, '^')
               ),
      prec(290,
           seq(repeat1('!'), $.expression)
          )
    )),

    expr_application: $ => prec(300, seq(
      $.expression,
      '(',
      sep($._expr_argument, ','),
      ')'
    )),

    _expr_argument: $ => choice(
      $.expression,
      $.expr_named_argument
    ),

    expr_named_argument: $ => seq(
      $._variable_name, '=', $.expression
    ),

    _expr_atom: $ => prec(10000, choice(
      seq('(', $.expression, ')'),
      $.variable,
      $.constructor,
      $.bytes,
      $.address,
      $.lambda_op,
      $.integer,
      $.bool,
      $.record,
      $.expr_projection,
      $.map,
      $.map_access,
      $.empty_map_or_record,
      $._list,
      $.tuple,
      $.hole
    )),

    bytes: $ => $._lex_bytes,

    address: $ => $._lex_address,

    lambda_op: $ => seq(
      '(',
      choice('&&', '||',
             '+', '-', '*', '/', '^', 'mod',
             '==', '!=', '<', '>', '<=', '=<', '>=',
             '::', '++', '|>',
             '!'
            ),
      ')'
    ),

    integer: $ => choice(
      field("dec", $._lex_int_dec),
      field("hex", $._lex_int_hex),
    ),

    bool: $ => choice('true', 'false'),

    record: $ => seq(
      '{', repeat1($._record_field), '}'
    ),

    _record_field: $ => seq(
      $._field_name, '=', $.expression
    ),

    expr_record_update: $ => prec(400, seq(
      $.expression,
      '{', repeat1($._record_field_update), '}'
    )),

    _record_field_update: $ => seq(
      sep1($._field_name, '.'),
      field("old_value", optional(seq('@', $._variable_name))),
      '=', $.expression
    ),

    expr_projection: $ => prec(500, seq(
      $.expression, '.', $._field_name
    )),

    map: $ => seq(
      '{', repeat1($._map_field), '}'
    ),

    _map_field: $ => seq(
      $.map_access, '=',
      field("old_value", optional(seq('@', $._variable_name))),
      $.expression
    ),

    expr_map_update: $ => prec(400, seq(
      $.expression,
      '{', repeat1($._map_field_update), '}'
    )),

    _map_field_update: $ => seq(
      $.map_access, '=',
      field("old_value", optional(seq('@', $._variable_name))),
      $.expression
    ),

    map_access: $ => seq(
      '[',
      $.expression,
      field("default_value", optional(seq('=', $.expression))),
      ']',
    ),

    empty_map_or_record: $ => seq('{', '}'),

    _list: $ => choice(
      $.list_literal,
      $.list_range,
      $.list_comprehension
    ),

    list_literal: $ => seq(
      '[',
      sep($.expression, ','),
      ']'
    ),

    list_range: $ => seq(
      '[', $.expression, '..', $.expression, ']'
    ),

    list_comprehension: $ => seq(
      '[', $.expression, '|',
      sep1($._list_comprehension_expr, ','),
      ']'
    ),

    _list_comprehension_expr: $ => choice(
      $.list_comprehension_bind,
      $.list_comprehension_decl,
      $.list_comprehension_if
    ),


    list_comprehension_bind: $ => seq(
      $._pattern, '<-', $.expression
    ),

    list_comprehension_decl: $ => seq(
      'let', $._pattern, '=', $.expression
    ),

    list_comprehension_if: $ => seq(
      'if', '(', $.expression, ')'
    ),

    tuple: $ => seq(
      '(', sep2($.expression, ','), ')'
    ),

    hole: $ => '_',

    expr_if : $ => seq(
      'if', '(', $.expression, ')', $.expression,
      repeat(seq('elif', '(', $.expression, ')', $.expression)),
      'else', $.expression
    ),

    expr_switch: $ => seq(
      'switch', '(', $.expression, ')',
      block($, repeat($._switch_case))
    ),

    _switch_case: $ => seq(
      $._pattern, $._switch_branch
    ),

    _switch_branch: $ => choice(
      seq('=>', $.expression),
      $.guarded_branch
    ),

    guarded_branch: $ => repeat1(
      seq('|', sep1($.expression, ','), '=>', $.expression)
    ),

    expr_block: $ => block(
      $, $.statement
    ),

    _pattern: $ => choice(
      $._variable_name // TODO
    ),


    statement: $ => choice(
      $.expression,
      $.let_definition,
      $.statement_if
    ),

    statement_if: $ => seq(
      'if', '(', $.expression, ')', $.expression,
      repeat(seq('elif', '(', $.expression, ')', $.expression))
    ),

    let_definition: $ => seq(
      'let', $._pattern,
      optional(seq('(', sep($._pattern, ','), ')')),
      optional(seq(':', $.type)), '=',
      $.expression
    ),


    type: $ => choice(
      $.type_function,
      $.type_tuple,
      $.type_application,
      $._type_atom,
    ),

    type_function: $ => prec.right(seq(
      $.type_domain, '=>', $.type
    )),

    type_domain: $ => prec(150, choice(
      // Zero arguments
      seq('(', ')'),
      // Single argument
      $.type,
      // Multiple arguments
      seq(
        '(',
        sep2($.type, ','),
        ')'
      )
    )),

    type_tuple: $ => prec.left(100, sep2($.type, '*')),

    type_application: $ => prec.left(200, seq(
      $.type,
      '(',
      sep($.type, ','),
      ')'
    )),

    _type_atom: $ => choice(
      $.type_variable,
      $.variable,
      seq('(', $.type, ')')
    ),


    variable: $ => $._lex_qual_low_id,

    constructor: $ => $._lex_qual_up_id,

    type_variable: $ => $._lex_typevar,

    _name: $ => choice($._lex_low_id, $._lex_up_id),

    _variable_name: $ => $._lex_low_id,

    _type_variable_name: $ => $._lex_typevar,

    _field_name: $ => $._lex_low_id,

    _function_name: $ => $._lex_low_id,

    _type_name: $ => $._lex_low_id,

    _constructor_name: $ => $._lex_up_id,

    _scope: $ => $._lex_qual_up_id,

    _scope_name: $ => $._lex_up_id,

    _lex_int_dec: $ => token(/0|[1-9](_?[0-9]+)*/),
    _lex_int_hex: $ => token(/0x([0-9a-fA-F](_?[0-9a-fA-F]+)*)/),

    _lex_low_id: $ => token(/[a-z_][a-zA-Z0-9_]*/),
    _lex_up_id: $ => token(/[A-Z][a-zA-Z0-9_]*/),

    _lex_qual_low_id: $ => token(/([A-Z][a-zA-Z0-9_]*\.)*[a-z_][a-zA-Z0-9_]*/),
    _lex_qual_up_id: $ => token(/([A-Z][a-zA-Z0-9_]*\.)*[A-Z][a-zA-Z0-9_]*/),

    _lex_typevar: $ => token(/'*[a-z_][a-zA-Z0-9_]*/),

    _lex_bytes: $ => token(/#[0-9a-fA-F]{2}(_?[0-9a-fA-F]{2})*/),

    _lex_address: $ => token(/((ak)|(ok)|(oq)|(ct))_[0-9a-zA-Z]+/),

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

    block_open: $ => 'begin',
    block_close: $ => 'end',
    block_semi: $ => ';'

    // block_open: $ => $._indent,
    // block_close: $ => $._dedent,
    // block_semi: $ => $._newline
  }
});


function block($, rule) {
  return seq(
    $.block_open,
    repeat1(seq(rule, $.block_semi)),
    $.block_close
  )
}

function maybe_block($, rule) {
  return choice(
    block($, rule),
    rule
  )
}

function sep(rule, delimiter) {
  return optional(sep1(rule, delimiter))
}

function sep1(rule, delimiter) {
  return seq(rule, repeat(seq(delimiter, rule)))
}

function sep2(rule, delimiter) {
  return seq(rule, repeat1(seq(delimiter, rule)))
}

function _expr_op($, rule_op) {
  return seq($.expression, rule_op, $.expression)
}
