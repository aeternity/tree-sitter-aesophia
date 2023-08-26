const OP_ASSOC = {
  'OP_PIPE': 'left',
  'OP_DISJ': 'right',
  'OP_CONJ': 'right',
  'OP_CMP': 'left',
  'OP_LIST': 'right',
  'OP_ADD': 'left',
  'OP_NEG': 'unary',
  'OP_MUL': 'left',
  'OP_POW': 'left',
  'OP_NOT': 'unary',
};

module.exports = grammar({
  name: 'aesophia',

  conflicts: $ => [
    // Both match (pat, pat), but they never coexist because pat_args requires something definite
    // before.
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
    /[\s\uFEFF\u2060\u200B]/,

  ],

  externals: $ => [
    $._block_open,
    $._block_indent,
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
    [ // expressions

      'EXPR_LAMBDA',
      'EXPR_ATOM',
      'EXPR_PROJECTION',
      'EXPR_APP',
      'EXPR_UPDATE_ACCESS',
      'EXPR_GUARD',
      'EXPR_TYPED',

      'OP_NOT',
      'OP_POW',
      'OP_MUL',
      'OP_NEG',
      'OP_ADD',
      'OP_LIST',
      'OP_CMP',
      'OP_CONJ',
      'OP_DISJ',
      'OP_PIPE',

      'EXPR_IF',
      'EXPR_SWITCH',
      'EXPR_BLOCK',
    ],
    [ // statements
      'STMT_EXPR',
      'STMT_IF',
      'STMT_LET',
    ],
    [ // types
      'TYPE_ATOM',
      'TYPE_APP',
      'TYPE_DOMAIN',
      'TYPE_TUPLE',
      'TYPE_FUN',
    ],
    [ // patterns
      'PAT_ATOM',
      'PAT_LIST',
      'PAT_OP',
      'PAT_LET',
      'PAT_APP',
      'PAT_TYPED',
    ],
    // conflicts
    [ 'EXPR_ATOM', 'PAT_ATOM' ],
    [ 'EXPR_TYPED', 'TYPE_DOMAIN' ],
    [ 'PAT_TYPED', 'TYPE_DOMAIN' ],
    [ 'EXPR_IF', 'STMT_IF' ],
    [ 'PAT_ARGS', 'EXPR_ATOM'],
  ],

  word: $ => $._lex_low_id,

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
      field("body", $._expression_body)
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

    _expression_body: $ => prec('EXPR_BLOCK', choice(
      // $.expr_block,
      $._expression,
    )),

    _expression: $ => choice(
      $.expr_lambda,
      // $.expr_typed,
      // $.expr_op,
      // $.expr_application,
      // $.expr_record_update,
      // $.expr_map_update,
      // $.expr_map_access,
      // $.expr_projection,
      // $.expr_switch,
      // $.expr_if,
      $._expr_atom
    ),

    expr_lambda: $ => prec.right('EXPR_LAMBDA', seq(
      field("args", $.pat_args),
      '=>',
      field("body", $._expression_body)
    )),

    expr_op: $ => choice(
      ...[
        [$.op_pipe, 'OP_PIPE'],
        [$.op_or,   'OP_DISJ'],
        [$.op_and,  'OP_CONJ'],
        [$.op_lt,   'OP_CMP'],
        [$.op_gt,   'OP_CMP'],
        [$.op_le,   'OP_CMP'],
        [$.op_ge,   'OP_CMP'],
        [$.op_eq,   'OP_CMP'],
        [$.op_neq,  'OP_CMP'],
        [$.op_cons, 'OP_LIST'],
        [$.op_cat,  'OP_LIST'],
        [$.op_add,  'OP_ADD'],
        [$.op_sub,  'OP_ADD'],
        [$.op_sub,  'OP_NEG'],
        [$.op_mul,  'OP_MUL'],
        [$.op_div,  'OP_MUL'],
        [$.op_mod,  'OP_MUL'],
        [$.op_pow,  'OP_POW'],
        [$.op_not,  'OP_NOT'],
      ].map(([operator, precedence]) =>
        (OP_ASSOC[precedence] === 'left' ? prec.left :
         OP_ASSOC[precedence] === 'right' ? prec.right :
         prec
        )(
          precedence,
          OP_ASSOC[precedence] === 'unary' ? seq(
            field("op", operator),
            field("op_r", $._expression),
          ) : seq(
            field("op_l", $._expression),
            field("op", operator),
            field("op_r", $._expression_body)
          )
        )
      )
    ),

    expr_typed: $ => prec.left('EXPR_TYPED', seq(
      field("expr", $._expression), ':',
      field("type", $._type)
    )),

    expr_application: $ => prec.left('EXPR_APP', seq(
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

    expr_record_update: $ => prec.left('EXPR_UPDATE_ACCESS', seq(
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

    expr_map_update: $ => prec.left('EXPR_UPDATE_ACCESS', seq(
      field("map", $._expression),
      field("updates", $.map_updates),
    )),

    map_updates: $ => seq(
      '{',
      sep1(field("update", $.map_update), ','),
      '}'
    ),

    map_update: $ => seq(
      field("key", $.expr_map_key), '=',
      optional(seq('@', field("old_value", $.variable_name))),
      field("new_value", $._expression)
    ),

    expr_map_access: $ => prec.left('EXPR_UPDATE_ACCESS', seq(
      field("map", $._expression),
      field("key", $.expr_map_key)
    )),

    expr_map_key: $ => seq(
      '[',
      field("key", $._expression),
      optional(seq('=', field("default_value", $._expression))),
      ']',
    ),

    expr_projection: $ => prec.left('EXPR_PROJECTION', seq(
      field("expr", $._expression), '.',
      field("field", $.field_name)
    )),

    expr_if: $ => prec.right('EXPR_IF', seq(
      'if', '(',
      field("cond", $._expression), ')',
      field("then", $._expression_body),
      repeat($._expr_elif),
      seq('else', field("else", $._expression_body))
      // `else` is optional in statements
      // optional(seq('else', field("else", $._expression)))
    )),

    _expr_elif: $ => prec('EXPR_IF', seq(
      'elif', '(',
      field("cond", $._expression), ')',
      field("then", $._expression_body)
    )),

    expr_switch: $ => prec('EXPR_SWITCH', seq(
      'switch', '(',
      field("expr", $._expression), ')',
      field("cases", $.expr_cases)
    )),

    expr_cases: $ => maybe_block($, field("case", $.switch_case)),

    switch_case: $ => seq(
      field("pattern", $._pattern),
      field("branch", $._switch_branch)
    ),

    _switch_branch: $ => prec.right(choice(
      $.unguarded_branch,
      $.guarded_branches,
    )),

    unguarded_branch: $ => prec('EXPR_GUARD', seq(
      '=>',
      field("body", $._expression)
    )),

    guarded_branches: $ => maybe_block($, seq('|', field("branch", $.guarded_branch))),

    guarded_branch: $ => prec('EXPR_GUARD', seq(
      sep1(field("guards", $._expression), ','), '=>',
      field("body", $._expression)
    )),

    expr_block: $ => prec('EXPR_BLOCK', block(
      $, field("stmt", $._statement)
    )),

    _expr_atom: $ => choice(
      $.expr_variable,
      $.expr_record,
      $.expr_map,
      $._expr_list,
      $.expr_tuple,
      $.expr_literal,
      $.expr_paren,
    ),

    expr_variable: $ => prec('EXPR_ATOM', $._lex_qual_low_id),

    expr_record: $ => prec('EXPR_ATOM', seq(
      '{',
      sep1(field("field", $.expr_record_field), ','),
      '}'
    )),

    expr_record_field: $ => seq(
      field("name", $.field_name), '=',
      field("value", $._expression)
    ),

    expr_map: $ => prec('EXPR_ATOM', seq(
      '{',
      sep1(field("field", $.map_field), ','),
      '}'
    )),

    map_field: $ => seq(
      field("key", $.expr_map_key), '=',
      optional(seq('@', field("old_value", $._variable_name))),
      field("new_value", $._expression)
    ),

    _expr_list: $ => choice(
      $.expr_list_literal,
      $.expr_list_range,
      $.expr_list_comprehension
    ),

    expr_list_literal: $ => prec('EXPR_ATOM', seq(
      '[',
      sep(seq(field("elem", $._expression), optional($._block_semi)), ','),
      ']'
    )),

    expr_list_range: $ => prec('EXPR_ATOM', seq(
      '[',
      field("start", $._expression), '..',
      field("end", $._expression), ']'
    )),

    expr_list_comprehension: $ => prec('EXPR_ATOM', seq(
      '[',
      field("yield", $._expression), '|',
      sep1(field("filter", $._list_comprehension_filter), ','),
      ']'
    )),

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
      field("cond", $._expression),
      ')'
    ),

    expr_tuple: $ => prec('EXPR_ATOM', seq(
      '(',
      sep2(field("elem", $._expression), ','),
      ')'
    )),

    expr_literal: $ => prec('EXPR_ATOM', $._literal),

    expr_paren: $ => prec('EXPR_ATOM', seq(
      '(',
      field("expr", $._expression),
      ')',
    )),

    //**************************************************************************
    // PATTERN
    //**************************************************************************

    _pattern: $ => prec('PAT_ATOM', choice(
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
    )),

    pat_typed: $ => prec.left('PAT_TYPED', seq(
      field("pattern", $._pattern), ':',
      field("type", $._type)
    )),

    pat_application: $ => prec.left('PAT_APP', seq(
      field("fun", $._pattern),
      field("args", $.pat_args),
    )),

    pat_args: $ => prec('PAT_ARGS', seq(
      '(',
      sep(field("arg", $._pattern), ','),
      ')'
    )),

    pat_let: $ => prec.left('PAT_LET', seq(
      field("name", $.pat_variable), '=',
      field("pattern", $._pattern)
    )),

    pat_operator: $ => prec.right('PAT_OP', seq(
      field("op_l", $._pattern), '::',
      field("op_r", $._pattern)
    )),

    pat_literal: $ => prec('PAT_ATOM', $._literal),

    pat_variable: $ => prec('PAT_ATOM', $._variable_name),

    pat_list: $ => prec('PAT_ATOM', seq(
      '[',
      sep(field("elem", $._pattern), ','), ']'
    )),

    pat_tuple: $ => prec('PAT_ATOM', seq(
      '(',
      sep2(field("elem", $._pattern), ','),
      ')'
    )),

    pat_record: $ => prec('PAT_ATOM', seq(
      '{',
      sep1(field("field", $.pat_record_field), ','),
      '}'
    )),

    pat_record_field: $ => seq(
      field("path", $.field_path), '=',
      field("pattern", $._pattern)
    ),

    pat_map: $ => prec('PAT_ATOM', seq(
      '{',
      sep1(field("field", $.pat_map_field), ','),
      '}'
    )),

    pat_map_field: $ => seq(
      '[',
      field("key", $._expression), ']', '=',
      field("pattern", $._pattern),
    ),

    pat_parens: $ => prec('PAT_ATOM', seq(
      '(',
      field("pattern", $._pattern),
      ')'
    )),

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
      $.stmt_if,
      $.stmt_expr,
    ),

    stmt_let: $ => prec('STMT_LET', seq(
      'let',
      field("pattern", $._pattern),
      field("args", optional($.pat_args)),
      optional(seq(':', field("type", $._type))), '=',
      field("value", $._expression)
    )),


    stmt_if: $ => prec.right('STMT_IF', seq(
      'if', '(',
      field("cond", $._expression), ')',
      field("then", $._expression_body),
      repeat($._stmt_elif),
      // `else` is optional in statements
    )),

    _stmt_elif: $ => prec('STMT_IF', seq(
      'elif', '(',
      field("cond", $._expression), ')',
      field("then", $._expression_body)
    )),


    stmt_expr: $ => prec('STMT_EXPR', $._expression),

    //**************************************************************************
    // TYPE
    //**************************************************************************

    _type: $ => prec('TYPE_ATOM', choice(
      $.type_function,
      $.type_tuple,
      $._type_in_tuple
    )),

    type_function: $ => prec.right('TYPE_FUN', seq(
      field("domain", $._type_domain),
      '=>',
      field("codomain", $._type)
    )),

    _type_domain: $ => prec('TYPE_DOMAIN', choice(
      $.type_domain_zero,
      $.type_domain_one,
      $.type_domain_many
    )),

    type_domain_zero: $ => seq('(', ')'),

    type_domain_one: $ => prec('TYPE_DOMAIN', field("param", $._type)),

    type_domain_many: $ => seq(
      '(',
      sep2(field("param", $._type), ','),
      ')'
    ),

    type_tuple: $ => prec.left('TYPE_TUPLE', sep2(field("elem", $._type_in_tuple), '*')),

    _type_in_tuple: $ => choice(
      $.type_application,
      $._type_atom,
    ),

    type_application: $ => prec.left('TYPE_APP', seq(
      field("fun", $.type_variable),
      field("params", $.type_params),
    )),

    type_params: $ => seq(
      '(',
      sep(field("param", $._type), ','),
      ')'
    ),

    _type_atom: $ => prec('TYPE_ATOM', choice(
      $.type_variable_poly,
      $.type_variable,
      $.type_paren,
    )),

    type_paren: $ => seq(
      '(',
      field("type", $._type),
      ')'
    ),

    type_variable_poly: $ => $._lex_prim_id,

    type_variable: $ => $._lex_qual_low_id,

    //**************************************************************************
    // OPERATORS
    //**************************************************************************

    _operator: $ => choice(
      $.op_pipe,
      $.op_or,
      $.op_and,
      $.op_lt,
      $.op_gt,
      $.op_le,
      $.op_ge,
      $.op_eq,
      $.op_neq,
      $.op_cons,
      $.op_cat,
      $.op_add,
      $.op_sub,
      $.op_mul,
      $.op_div,
      $.op_mod,
      $.op_pow,
      $.op_not,
    ),

    op_pipe: $ => '|>',
    op_or: $ => '||',
    op_and: $ => '&&',
    op_lt: $ => '<',
    op_gt: $ => '>',
    op_le: $ => '=<',
    op_ge: $ => '>=',
    op_eq: $ => '==',
    op_neq: $ => '!=',
    op_cons: $ => '::',
    op_cat: $ => '++',
    op_add: $ => '+',
    op_sub: $ => '-',
    op_mul: $ => '*',
    op_div: $ => '/',
    op_mod: $ => 'mod',
    op_pow: $ => '^',
    op_not: $ => '!',

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
  }
});


function block($, rule) {
  return seq(
    $._block_open,
    $._block_indent,
    sep1(rule, $._block_semi),
    $._block_close
  );
}

function block_or($, elem, single) {
  return choice(
    block($, elem),
    single
  );
}

function maybe_block($, rule) {
  return block_or($, rule, rule);
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

function dispath($, trigger, rule) {
  return seq(
    $._lex_dispath_begin,
    trigger,
    $._lex_dispath_end,
    rule,
  );
}
