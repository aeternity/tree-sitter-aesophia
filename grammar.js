const OP = {
  '|>': 'PIPE',
  '||': 'DISJ',
  '&&': 'CONJ',
  '<':  'CMP',
  '>':  'CMP',
  '=<': 'CMP',
  '>=': 'CMP',
  '==': 'CMP',
  '!=': 'CMP',
  '::': 'LIST',
  '++': 'LIST',
  '+':  'ADD',
  '-':  ['ADD', 'NEG'],
  '*':  'MUL',
  '^':  'POW',
  '!':  'NOT',
};

module.exports = grammar({
  name: 'aesophia',

  conflicts: $ => [
    // Both match (pat, pat), but they never coexist because pat_args requires something before.
    [$.pat_args, $.pat_tuple],
  ],

  words: $ => [
    _lex_low_id,
    _lex_up_id,
  ],

  extras: $ => [
    $.doc_comment,
    $.block_comment,
    $.line_comment,
    /[\s\uFEFF\u2060\u200B]|\\\r?\n/,
  ],

  externals: $ => [
    $._block_open,
    $._block_semi,
    $._block_close,
    $._block_comment_content,
    $._error_state,
  ],

  supertypes: $ => [
    $._expression,
    $._statement,
    $._pattern,
    $._type,
  ],

  precedences: $ => [
    [ // operators
      'PIPE': {args: 2, bind: 'left', prec: 0},
      'DISJ': {args: 2, bind: 'right', prec: 10},
      'CONJ': {args: 2, bind: 'right', prec: 20},
      'CMP':  {args: 2, bind: 'left', prec: 30},
      'LIST': {args: 2, bind: 'right', prec: 40},
      'ADD':  {args: 2, bind: 'left', prec: 50},
      'NEG':  {args: 1, bind: 'none', prec: 60},
      'MUL':  {args: 2, bind: 'left', prec: 70},
      'POW':  {args: 2, bind: 'left', prec: 80},
      'NOT':  {args: 2, bind: 'none', prec: 90},
    ]
  ],

  rules: {
    source: $ => choice(
      dispath($, 'PATTERN', $._pattern),
      dispath($, 'TYPE', $._type),
      dispath($, 'LITERAL', $._literal),
      dispath($, 'EXPRESSION', $._expression),
      dispath($, 'STATEMENT', $._statement),
      $._top_level
    ),

    _top_level: $ => repeat1($._top_decl),

    _top_decl: $ => choice(
      $.top_pragma,
      $.include,
      $.using,
      $.scope_declaration
    ),

    top_pragma: $ => 'TODO',

    using: $ => seq(
      'using',
      field("scope", $.scope),
      optional(choice(
        $.using_hiding,
        $.using_for,
      ))
    ),

    using_hiding: $ => seq(
      'hiding',
      field("names", $.using_name_list)
    ),

    using_for: $ => seq(
      'for',
      field("names", $.using_name_list)
    ),

    using_name_list: $ => seq(
      '[',
      sep(field("name", $._name), ','),
      ']'
    ),

    include: $ => seq(
      'include',
      field("path", $.include_path)
    ),

    include_path: $ => $._lex_string,

    scope_declaration: $ => seq(
      field("modifiers", repeat($.scope_modifier)),
      field("header", $.scope_header),
      field("interface", optional('interface')),
      field("name", $.scope_name),
      '=',
      maybe_block($, field("decl", $._scoped_declaration))
    ),

    scope_modifier: $ => choice(
      'main', 'payable'
    ),

    scope_header: $ => choice(
      'contract', 'namespace'
    ),

    _scoped_declaration: $ => choice(
      $._type_definition,
      $.function_declaration,
    ),

    function_declaration: $ => seq(
      field("modifiers", repeat(field("modifier", $._function_modifier))),
      field("head", $.function_head),
      maybe_block($, field("clause",
                           choice(
                             $.function_signature,
                             $.function_clause
                           )))
    ),

    function_signature: $ => seq(
      field("name", $.function_name), ':',
      field("type", $._type)
    ),

    function_clause: $ => seq(
      field("name", $.function_name),
      field("args", $.pat_args),
      field("ret_type", optional(seq(':', $._type))),
      '=',
      field("body", $._expression)
    ),

    function_head: $ => choice(
      'function',
      'entrypoint'
    ),

    _function_modifier: $ => choice(
      'payable',
      'stateful',
      'private'
    ),

    //**************************************************************************
    // TYPE DECLARATION
    //**************************************************************************

    _type_definition: $ => choice(
      $.type_alias,
      $.record_declaration,
      $.variant_declaration
    ),

    type_param_decls: $ => seq(
      '(',
      sep(field("name", $.type_variable_poly_name), ','),
      ')'
    ),

    type_alias: $ => seq(
      'type',
      field("name", $.type_name),
      field("params", optional($.type_param_decls)),
      '=',
      field("type", $._type)
    ),

    record_declaration: $ => seq(
      'record',
      field("name", $.type_name),
      field("params", optional($.type_param_decls)),
      '=',
      field("fields", $.record_fields),
    ),

    record_fields: $ => seq(
      '{',
      sep(field("field", $.field_declaration), ','),
      '}'
    ),

    field_declaration: $ => seq(
      field("name", $.field_name),
      ':',
      field("type", $._type)
    ),

    variant_declaration: $ => seq(
      'datatype',
      field("name", $.type_name),
      field("params", optional($.type_param_decls)),
      '=',
      sep1(field("constructor", $.constructor_declaration), '|')
    ),

    constructor_declaration: $ => seq(
      field("name", $.constructor_name),
      field("params", optional($.type_params))
    ),

    //**************************************************************************
    // EXPRESSION
    //**************************************************************************

    _expression: $ => choice(
      $.expr_lambda,
      $.expr_typed,
      $.expr_op,
      $.expr_application,
      $.expr_record_update,
      $.expr_map_update,
      $.expr_map_access,
      $.expr_projection,
      $.expr_switch,
      $.expr_if,
      $.expr_block,
      $._expr_atom
    ),

    expr_lambda: $ => prec.right(1000, seq(
      field("args", $.pat_args),
      '=>',
      field("body", $._expression)
    )),

    expr_typed: $ => prec.left(1100, seq(
      field("expr", $._expression), ':',
      field("type", $._type)
    )),

    expr_op: $ => prec(1200, choice(
      prec.left(1200, _expr_op($, '|>')
               ),
      prec.right(1210,
                 _expr_op($, '||')
                ),
      prec.right(1220,
                 _expr_op($, '&&')
                ),
      prec.left(1230,
           _expr_op($, choice('<', '>', '=<', '>=', '==', '!='))
          ),
      prec.right(1240,
                 _expr_op($, choice('::', '++'))
                ),
      prec.left(1250,
                _expr_op($, choice('+', '-'))
               ),
      prec(1260,
           seq('-', $._expression)
          ),
      prec.left(1270,
                _expr_op($, choice('*', '/', 'mod'))
               ),
      prec.left(1280,
                _expr_op($, '^')
               ),
      prec(1290,
           seq('!', $._expression)
          )
    )),

    expr_application: $ => prec.left(1300, seq(
      field("fun", $._expression),
      field("args", $.expr_args),
    )),

    expr_args: $ => seq(
      '(',
      sep(field("arg", $._expr_argument), ','),
      ')',
    ),

    _expr_argument: $ => choice(
      $._expression,
      $.expr_named_argument
    ),

    expr_named_argument: $ => seq(
      field("name", $.variable_name), '=',
      field("value", $._expression)
    ),

    expr_record_update: $ => prec(1400, seq(
      field("record", $._expression),
      field("updates", $.record_field_updates),
    )),

    record_field_updates: $ => seq(
      '{',
      sep(field("update", $.record_field_update), ','),
      '}'
    ),

    record_field_update: $ => seq(
      field("path", $.field_path),
      optional(seq('@', field("old_value", $.variable_name))),
      '=',
      field("new_value", $._expression)
    ),

    field_path: $ => prec.left(seq(
      sep1(field("field", $.field_name), '.'),
    )),

    expr_map_update: $ => prec(1400, seq(
      field("map", $._expression),
      field("updates", $.map_updates),
    )),

    map_updates: $ => seq(
      '{',
      repeat1(field("update", $.map_update)),
      '}'
    ),

    map_update: $ => seq(
      $._expr_map_key, '=',
      optional(seq('@', field("old_value", $.variable_name))),
      field("new_value", $._expression)
    ),

    expr_map_access: $ => prec(1400, seq(
      field("map", $._expression),
      $._expr_map_key
    )),

    _expr_map_key: $ => seq(
      '[',
      field("key", $._expression),
      optional(seq('=', field("default_value", $._expression))),
      ']',
    ),

    expr_if: $ => prec.right(1000, seq(
      'if', '(',
      field("cond", $._expression), ')',
      field("then", $._expression),
      repeat($._expr_elif),
      // `else` is optional in statements
      optional(seq('else', field("else", $._expression)))
    )),

    _expr_elif: $ => seq(
      'elif', '(',
      field("cond", $._expression), ')',
      field("then", $._expression)
    ),

    expr_switch: $ => seq(
      'switch', '(',
      field("expr", $._expression), ')',
      field("cases", $.expr_cases)
    ),

    expr_cases: $ => maybe_block($, field("case", $.switch_case)),

    switch_case: $ => seq(
      field("pattern", $._pattern),
      field("branch", $._switch_branch)
    ),

    _switch_branch: $ => prec.right(choice(
      $.unguarded_branch,
      $.guarded_branches,
    )),

    unguarded_branch: $ => seq(
      '=>',
      field("body", $._expression)
    ),

    guarded_branches: $ => maybe_block($, seq('|', field("branch", $.guarded_branch))),

    guarded_branch: $ => seq(
      sep1(field("guards", $._expression), ','), '=>',
      field("body", $._expression)
    ),

    expr_block: $ => block(
      $, field("stmt", $._statement)
    ),

    _expr_atom: $ => prec(10000, choice(
      $.expr_variable,
      $.expr_record,
      $.expr_projection,
      $.expr_map,
      $._expr_list,
      $.expr_tuple,
      $.expr_literal,
      $.expr_paren,
    )),

    expr_variable: $ => $._lex_qual_low_id,

    expr_record: $ => seq(
      '{',
      repeat1(field("field", $.expr_record_field)), '}'
    ),

    expr_record_field: $ => seq(
      field("name", $.field_name), '=',
      field("value", $._expression)
    ),

    expr_projection: $ => prec.left(1500, seq(
      field("expr", $._expression), '.',
      field("field", $.field_name)
    )),

    expr_map: $ => seq(
      '{',
      repeat1(field("field", $.map_field)), '}'
    ),

    map_field: $ => seq(
      field("key", $._expr_map_key), '=',
      optional(seq('@', field("old_value", $._variable_name))),
      field("new_value", $._expression)
    ),

    _expr_list: $ => prec(10000, choice(
      $.expr_list_literal,
      $.expr_list_range,
      $.expr_list_comprehension
    )),

    expr_list_literal: $ => seq(
      '[',
      sep(field("elem", $._expression), ','),
      ']'
    ),

    expr_list_range: $ => seq(
      '[',
      field("start", $._expression), '..',
      field("end", $._expression), ']'
    ),

    expr_list_comprehension: $ => seq(
      '[',
      field("yield", $._expression), '|',
      sep1(field("filter", $._list_comprehension_filter), ','),
      ']'
    ),

    _list_comprehension_filter: $ => choice(
      $.list_comprehension_bind,
      $.list_comprehension_decl,
      $.list_comprehension_if
    ),

    list_comprehension_bind: $ => seq(
      field("pattern", $._pattern), '<-',
      field("expr", $._expression)
    ),

    list_comprehension_decl: $ => seq(
      'let',
      field("pattern", $._pattern), '=',
      field("expr", $._expression)
    ),

    list_comprehension_if: $ => seq(
      'if', '(',
      field("cond", $._expression), ')'
    ),

    expr_tuple: $ => prec(10000, seq(
      '(',
      sep2(field("elem", $._expression), ','),
      ')'
    )),

    expr_literal: $ => prec(10000, $._literal),

    expr_paren: $ => seq(
      '(',
      field("expr", $._expression),
      ')',
    ),

    //**************************************************************************
    // PATTERN
    //**************************************************************************

    _pattern: $ => choice(
      $.pat_typed,
      $.pat_application,
      $.pat_let,
      $.pat_literal,
      $.pat_variable,
      $.pat_list,
      $.pat_operator,
      $.pat_tuple,
      $.pat_record,
      $.pat_parens,
    ),

    pat_typed: $ => prec.left(100, seq(
      field("pattern", $._pattern), ':',
      field("type", $._type)
    )),

    pat_application: $ => prec.left(200, seq(
      field("fun", $._pattern),
      field("args", $.pat_args),
    )),

    pat_args: $ => seq(
      '(',
      sep(field("arg", $._pattern), ','),
      ')'
    ),

    pat_let: $ => prec.left(200, seq(
      field("name", $.pat_variable), '=',
      field("pattern", $._pattern)
    )),

    pat_operator: $ => prec.right(300, seq(
      field("op_l", $._pattern), '::',
      field("op_r", $._pattern)
    )),

    pat_literal: $ => prec(1000, $._literal),

    pat_variable: $ => $._variable_name,

    pat_list: $ => prec(1000, seq(
      '[',
      sep(field("elem", $._pattern), ','), ']'
    )),

    pat_tuple: $ => seq(
      '(',
      sep2(field("elem", $._pattern), ','),
      ')'
    ),

    pat_record: $ => seq(
      '{',
      sep1(field("field", $.pat_record_field), ','),
      '}'
    ),

    pat_record_field: $ => seq(
      field("path", $.field_path), '=',
      field("pattern", $._pattern)
    ),

    pat_map: $ => seq(
      '{',
      sep1(field("field", $.pat_map_field), ','),
      '}'
    ),

    pat_map_field: $ => seq(
      '[',
      field("key", $._expression), ']', '=',
      field("pattern", $._pattern),
    ),

    pat_parens: $ => seq(
      '(',
      field("pattern", $._pattern),
      ')'
    ),

    //**************************************************************************
    // LITERAL
    //**************************************************************************

    _literal: $ => choice(
      $.lit_constructor,
      $.lit_bytes,
      $.lit_address,
      $.lit_lambda_op,
      $.lit_integer,
      $.lit_bool,
      $.lit_empty_map_or_record,
      $.lit_string,
      $.lit_char,
      $.lit_wildcard
    ),

    lit_constructor: $ => $._lex_qual_up_id,

    lit_bytes: $ => $._lex_bytes,

    lit_address: $ => $._lex_address,

    lit_lambda_op: $ => seq(
      '(',
      field("op", $.op),
      ')'
    ),

    op: $ => choice(
      '&&', '||',
      '+', '-', '*', '/', '^', 'mod',
      '==', '!=', '<', '>', '<=', '=<', '>=',
      '::', '++', '|>',
      '!'
    ),

    lit_integer: $ => choice(
      $._lex_int_dec,
      $._lex_int_hex,
    ),

    lit_bool: $ => choice('true', 'false'),

    lit_empty_map_or_record: $ => seq('{', '}'),

    lit_string: $ => $._lex_string,

    lit_char: $ => $._lex_char,

    lit_wildcard: $ => $._lex_wildcard,

    //**************************************************************************
    // STATEMENT
    //**************************************************************************

    _statement: $ => choice(
      $.stmt_let,
      $.stmt_expr,
    ),

    stmt_let: $ => prec(10, seq(
      'let',
      field("pattern", $._pattern),
      field("args", optional($.pat_args)),
      optional(seq(':', field("type", $._type))), '=',
      field("value", $._expression)
    )),

    stmt_expr: $ => prec(30, $._expression),

    //**************************************************************************
    // TYPE
    //**************************************************************************

    _type: $ => choice(
      $.type_function,
      $.type_tuple,
      $._type_in_tuple
    ),

    type_function: $ => prec.right(100, seq(
      field("domain", $._type_domain),
      '=>',
      field("codomain", $._type)
    )),

    _type_domain: $ => prec(150, choice(
      $.type_domain_zero,
      $.type_domain_one,
      $.type_domain_many
    )),

    type_domain_zero: $ => seq('(', ')'),

    type_domain_one: $ => field("param", $._type),

    type_domain_many: $ => seq(
      '(',
      sep2(field("param", $._type), ','),
      ')'
    ),

    type_tuple: $ => prec(200, sep2(field("elem", $._type_in_tuple), '*')),

    _type_in_tuple: $ => prec(201, choice(
      $.type_application,
      $._type_atom,
    )),

    type_application: $ => prec.left(300, seq(
      field("fun", $._type_in_tuple),
      field("params", $.type_params),
    )),

    type_params: $ => seq(
      '(',
      sep(field("param", $._type), ','),
      ')'
    ),

    _type_atom: $ => choice(
      $.type_variable_poly,
      $.type_variable,
      $.type_paren,
    ),

    type_paren: $ => seq(
      '(',
      field("type", $._type),
      ')'
    ),

    type_variable_poly: $ => $._lex_prim_id,

    type_variable: $ => $._lex_qual_low_id,

    //**************************************************************************
    // NAMES
    //**************************************************************************

    _name: $ => choice($._lex_low_id, $._lex_up_id),
    name: $ => $._name,

    _variable_name: $ => $._lex_low_id,
    variable_name: $ => $._variable_name,

    _type_variable_poly_name: $ => $._lex_prim_id,
    type_variable_poly_name: $ => $._type_variable_poly_name,

    _field_name: $ => $._lex_low_id,
    field_name: $ => $._field_name,

    _function_name: $ => $._lex_low_id,
    function_name: $ => $._function_name,

    _type_name: $ => $._lex_low_id,
    type_name: $ => $._type_name,

    _constructor_name: $ => $._lex_up_id,
    constructor_name: $ => $._constructor_name,

    _scope: $ => $._lex_qual_up_id,
    scope: $ => $._scope,

    _scope_name: $ => $._lex_up_id,
    scope_name: $ => $._scope_name,

    //**************************************************************************
    // LEXEMES
    //**************************************************************************

    _lex_int_dec: $ => token(/0|[1-9](_?[0-9]+)*/),
    _lex_int_hex: $ => token(/0x([0-9a-fA-F](_?[0-9a-fA-F]+)*)/),

    _lex_low_id: $ => token(/[a-z_][a-zA-Z0-9_]*/),
    _lex_up_id: $ => token(/[A-Z][a-zA-Z0-9_]*/),

    _lex_qual_low_id: $ => token(/([A-Z][a-zA-Z0-9_]*\.)*[a-z_][a-zA-Z0-9_]*/),
    _lex_qual_up_id: $ => token(/([A-Z][a-zA-Z0-9_]*\.)*[A-Z][a-zA-Z0-9_]*/),

    _lex_prim_id: $ => token(/'*[a-z_][a-zA-Z0-9_]*/),

    _lex_bytes: $ => token(/#[0-9a-fA-F]{2}(_?[0-9a-fA-F]{2})*/),

    _lex_address: $ => token(/((ak)|(ok)|(oq)|(ct))_[0-9a-zA-Z]+/),

    _lex_wildcard: $ => token('_'),

    _lex_string: $ => token(/"([^"\\]|(\\.))*"/),

    _lex_char: $ => token(/'(([\x00-\x26\x28-\x5b\x5d-\x7f])|([\x00-\xff][\x80-\xff]{1,3})|(\\[befnrtv'\\])|(\\x[0-9a-fA-F]{2,2})|(\\x\{[0-9a-fA-F]*\}))'/),

    _lex_dispath_begin: $ => token.immediate(/@!/),
    _lex_dispath_end: $ => token.immediate(/.*\n/),

    doc_comment: ($) =>
    seq("/**", $._block_comment_content, "*/"),

    block_comment: ($) =>
    seq("/*", $._block_comment_content, "*/"),

    line_comment: ($) => token(seq(/\/\//, repeat(/[^\n]/))),

    _block_open: $ => 'begin',
    _block_close: $ => 'end',
    _block_semi: $ => ';'

    // _block_open: $ => $._indent,
    // _block_close: $ => $._dedent,
    // _block_semi: $ => $._newline
  }
});


function block($, rule) {
  return seq(
    $._block_open,
    sep1(rule, $._block_semi),
    $._block_close
  );
}

function maybe_block($, rule) {
  return choice(
    block($, rule),
    rule
  );
}

function sep(rule, delimiter) {
  return optional(sep1(rule, delimiter));
}

function sep1(rule, delimiter) {
  return seq(rule, repeat(seq(delimiter, rule)));
}

function sep2(rule, delimiter) {
  return seq(rule, repeat1(seq(delimiter, rule)));
}

function _expr_op($, rule_op) {
  return seq(
    field("op_l", $._expression),
    field("op", rule_op), // TODO this does not appear
    field("op_r", $._expression));
}

function dispath($, trigger, rule) {
  return seq(
    $._lex_dispath_begin,
    trigger,
    $._lex_dispath_end,
    rule,
  );
}
