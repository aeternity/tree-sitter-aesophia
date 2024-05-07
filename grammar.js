const sub = ($, rule) => seq(
  $._layout_start,
  rule,
  $._layout_end,
);

const block = ($, rule) => sub($, repeat1(seq(rule, $._layout_terminator)));

const weak_block = ($, rule) => sub($, repeat1(seq(rule, $._layout_terminator)));

const maybe_block = ($, rule) => choice(
  rule,
  block($, rule),
);


const sep2 = (rule, delimiter) => seq(
  rule,
  repeat1(seq(delimiter, rule))
);

const sep1 = (rule, delimiter) => seq(
  repeat(seq(rule, delimiter)),
  rule,
);

const sep = (rule, delimiter) => optional(sep1(rule, delimiter));

const lex_dispatch_begin = '@ts.parse(';
const lex_dispatch_end = ')\n';

const dispatch = ($, trigger, rule) => seq(
  lex_dispatch_begin,
  field("kind", alias(trigger, $.trigger)),
  lex_dispatch_end,
  field("content", rule),
);

const parens = ($, ...rule) => seq(
  '(',
  ...rule,
  $._paren_close,
);

const braces = ($, ...rule) => seq(
  '{',
  ...rule,
  $._brace_close
);

const brackets = ($, ...rule) => seq(
  '[',
  ...rule,
  $._bracket_close
);


const sep_comma = ($, rule) => seq(
  sep(rule, ','),
  optional(',')
);

const sep_comma1 = ($, rule) => seq(
  sep1(rule, ','),
  optional(',')
);

const sep_comma2 = ($, rule) => seq(
  sep2(rule, ','),
  optional(',')
);

const parens_comma = ($, rule) =>
      parens($, sep_comma($, rule));

const parens_comma1 = ($, rule) =>
      parens($, sep_comma1($, rule));

const parens_comma2 = ($, rule) =>
      parens($, sep_comma2($, rule));

const braces_comma = ($, rule) =>
      braces($, sep_comma($, rule));

const braces_comma1 = ($, rule) =>
      braces($, sep_comma1($, rule));

const braces_comma2 = ($, rule) =>
      braces($, sep_comma2($, rule));

const brackets_comma = ($, rule) =>
      brackets($, sep_comma($, rule));

const brackets_comma1 = ($, rule) =>
      brackets($, sep_comma1($, rule));

const brackets_comma2 = ($, rule) =>
      brackets($, sep_comma2($, rule));

const qual = (q, n) => seq(
  repeat(seq(q, token.immediate('.'))),
  n
);

const qual1 = (q, n) => seq(
  repeat1(seq(q, token.immediate('.'))),
  n
);


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
  ],

  extras: $ => [
    /[\n\r ]+/,
    $._synchronize,
  ],

  externals: $ => [
    $._block_comment_content,
    $._doc_comment_content,
    $.comment_content, // used to notify the scanner
    $._long_string_quote,
    $._layout_start,
    $._layout_end,
    $._layout_terminator,
    $._layout_at_level,
    $._layout_not_at_level,
    $._layout_empty,
    $._inhibit_layout_end,
    // @ts-ignore: DSL not updated for literals
    ",",
    "|",
    $._synchronize,
    $._invalid_layout,
    $._prefix_operator,
    $._symbol_export_marker,
  ],

  extras: $ => [
    /[\n\r ]+/,
    $._synchronize,
    $._line_comment,
    $._block_comment,
    $._doc_comment,
  ],

  inline: $ => [
    $._expr_list,
    $._pattern,
    $.pat_args,
    $._expr_sub,
    $._expression_weak_block
  ],

  precedences: $ => [
    [ // expressions
      'EXPR_ATOM',
      'EXPR_TUPLE',
      'EXPR_VAR',
      'EXPR_PROJECTION',
      'EXPR_APP',
      'EXPR_UPDATE_OR_ACCESS',
      'EXPR_MATCH',
      'EXPR_TYPED',
      'EXPR_BLOCK',

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
    ],

    [ // types
      'TYPE_ATOM',
      'TYPE_QUAL',
      'TYPE_APP',
      'TYPE_DOMAIN',
      'TYPE_TUPLE',
      'TYPE_FUN',
    ],

    // conflicts

    // (x)
    // this is somehow unclear whether it's a tuple or a beginning of a lambda
    [ $.expr_lambda, $._expression_simpl ],

    // switch(x) p | x : type => ...
    //               ^Should be parsed as typed guard, not x : function
    [ $.expr_typed, $.type_domain_one ],

    [$.expr_list_literal, $.expr_list_comprehension, ],
  ],

  word: $ => $._lex_low_id,

  rules: {
    source: $ => seq(
      choice(
        $._dispatch,
        field("module", $.module)
      ),
    ),

    _dispatch: $ => choice(
      dispatch($, 'module', $.module),
      dispatch($, 'top_decl', $._top_decl),
      dispatch($, 'type', $._type),
      dispatch($, 'literal', $._literal),
      dispatch($, 'expression', $._expression),
      dispatch($, 'scope_declaration', $.scope_declaration),
      dispatch($, 'function_declaration', $.function_declaration),
      dispatch($, '_scoped_declaration', $._scoped_declaration),
    ),

    _line_comment: $ => seq(
      $._line_comment_start,
      alias($.comment_content, $.line_comment),
    ),

    _block_comment: $ => seq(
      $._block_comment_start,
      alias($._block_comment_content, $.block_comment),
      $._block_comment_end,
    ),

    _doc_comment: $ => seq(
      $._doc_comment_start,
      alias($._doc_comment_content, $.doc_commment),
      $._block_comment_end,
    ),

    _line_comment_start: $ => token('//'),
    _block_comment_start: $ => token('/*'),
    _block_comment_end: $ => token('*/'),
    _doc_comment_start: $ => token('/**'),

    module: $ => repeat1(seq($._top_decl, $._layout_terminator)),

    _top_decl: $ => choice(
      $.top_pragma,
      $.include,
      $.using,
      $.scope_declaration
    ),

    top_pragma: $ => seq(
      '@',
      field("pragma", choice(
        $.pragma_compiler_vsn
      )),
    ),

    pragma_compiler_vsn: $ => seq(
      'compiler',
      field("op", choice(
        $.op_lt, $.op_le, $.op_gt, $.op_ge, $.op_eq, $.op_neq,
      )),
      field("version", $.version),
    ),

    version: $ => sep1(
      field("subver", alias($._lex_int_dec, $.subver)),
      '.'
    ),

    using: $ => seq(
      'using',
      field("scope", $.constructor),
      optional($.using_as),
      optional(field("select", choice(
        $.using_hiding,
        $.using_for,
      )))
    ),

    using_as: $ => seq(
      'as',
      field("alias", $.name)
    ),

    using_hiding: $ => seq(
      'hiding',
      field("names", $.using_name_list)
    ),

    using_for: $ => seq(
      'for',
      field("names", $.using_name_list)
    ),

    using_name_list: $ => brackets_comma1($,
      field("name", $.name)
    ),

    include: $ => seq(
      'include',
      field("path", $.include_path)
    ),

    include_path: $ => $._lex_string,

    scope_declaration: $ => seq(
      field("modifier", repeat($.scope_modifier)),
      field("head", $.scope_head),
      field("interface", alias(optional('interface'), $.is_interface)),
      field("name", $.constructor),
      optional(seq(":", sep1(field("implements", $.qual_constructor), ","))),
      '=',
      block($, field("decl", $._scoped_declaration))
    ),

    scope_modifier: $ => choice(
      'main', 'payable'
    ),

    scope_head: $ => choice(
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
      field("name", $.identifier), ':',
      field("type", $._type)
    ),

    function_clause: $ => seq(
      field("name", $.identifier),
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

    type_param_decls: $ => parens_comma($,
      field("name", $.type_variable_poly_name)
    ),

    type_alias: $ => seq(
      'type',
      field("name", $.identifier),
      field("params", optional($.type_param_decls)),
      '=',
      field("type", $._type)
    ),

    record_declaration: $ => seq(
      'record',
      field("name", $.identifier),
      field("params", optional($.type_param_decls)),
      '=',
      field("fields", $.record_fields),
    ),

    record_fields: $ => braces_comma($,
      field("field", $.field_declaration)
    ),

    field_declaration: $ => seq(
      field("name", $.identifier),
      ':',
      field("type", $._type)
    ),

    variant_declaration: $ => seq(
      'datatype',
      field("name", $.identifier),
      field("params", optional($.type_param_decls)),
      '=',
      sep1(field("constructor", $.constructor_declaration), '|')
    ),

    constructor_declaration: $ => seq(
      field("name", $.constructor),
      field("params", optional($.type_params))
    ),

    //**************************************************************************
    // EXPRESSION
    //**************************************************************************

    _expression_body: $ => prec('EXPR_BLOCK', choice(
      $._expr_sub,
      $.expr_block
    )),

    _expression_weak_block: $ => choice(
      $._expr_sub,
      block($, $._expression)
    ),

    _expr_sub: $ => seq(
      $._layout_not_at_level,
      $._expression,
    ),

    expr_block: $ => prec('EXPR_BLOCK', block($, $._expression)),

    _expression: $ => choice(
      $.expr_lambda,
      $._expr_if,
      $.expr_letval,
      $.expr_switch,
      $._expression_simpl
    ),

    _expression_simpl: $ => choice(
      $._expr_variable,
      $.expr_typed,
      $.expr_op,
      $.expr_application,
      $.expr_record_update,
      $.expr_map_update,
      $.expr_map_access,
      $.expr_projection,
      $.expr_tuple,
      $._expr_atom
    ),

    _expr_atom: $ => choice(
      $.expr_literal,
      $.expr_record,
      $.expr_map,
      $._expr_list,
      $.expr_hole,
      $.expr_wildcard,
    ),

    expr_lambda: $ => prec.right(seq(
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
            field("op_r", $._expression_weak_block)
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

    expr_args: $ => parens_comma($,
      field("arg", $.expr_argument)
    ),

    expr_argument: $ => prec('EXPR_MATCH', seq(
      optional(seq(field("name", $.identifier), '=')),
      field("value", $._expression)
    )),

    expr_record_update: $ => prec.left('EXPR_UPDATE_OR_ACCESS', seq(
      field("record", $._expression),
      field("updates", $.record_field_updates),
    )),

    record_field_updates: $ => braces_comma1($,
      field("update", $.record_field_update)
    ),

    record_field_update: $ => seq(
      field("path", $.field_path),
      optional(seq('@', field("old_value", $.identifier))),
      '=',
      field("new_value", $._expression)
    ),

    field_path: $ => prec.left(seq(
      sep1(field("field", $.identifier), '.'),
    )),

    expr_map_update: $ => prec.left('EXPR_UPDATE_OR_ACCESS', seq(
      field("map", $._expression),
      field("updates", $.map_updates),
    )),

    map_updates: $ => braces_comma1($,
      field("update", $.map_update)
    ),

    map_update: $ => seq(
      field("key", $.expr_map_key), '=',
      optional(seq('@', field("old_value", $.identifier))),
      field("new_value", $._expression)
    ),

    expr_map_access: $ => prec.left('EXPR_UPDATE_OR_ACCESS', seq(
      field("map", $._expression),
      field("key", $.expr_map_key)
    )),

    expr_map_key: $ => brackets($,
      field("key", $._expression),
      optional(seq('=', field("default_value", $._expression))),
    ),

    expr_projection: $ => prec.left('EXPR_PROJECTION', seq(
      field("expr", $._expression), '.',
      field("field", $.identifier)
    )),


    _expr_if: $ => choice(
      $.expr_if,
      $.expr_else
    ),

    expr_if: $ => prec.right(seq(
      'if',
      parens($, field("cond", $._expression)),
      field("then", $._expression_body),
      optional(seq($._layout_not_at_level, $.expr_else))
    )),

    expr_elif: $ => seq(
      'elif',
      parens($, field("cond", $._expression)),
      field("then", $._expression),
    ),

    expr_else: $ => seq(
      'else',
      field("else", $._expression_body)
    ),

    expr_switch: $ => seq(
      'switch',
      parens($, field("expr", $._expression)),
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
      field("body", $._expression_body),
    ),

    guarded_branches: $ => prec.right(repeat1(seq(
      '|', field("branch", $.guarded_branch)
    ))),

    guarded_branch: $ => seq(
      sep1(field("guard", $._expression), ','), '=>',
      field("body", $._expression_body)
    ),

    expr_letval: $ => seq(
      'let',
      field("pattern", $._pattern),
      '=',
      field("value", $._expression_body)
    ),


    // We allow tree sitter to parse commonly misplaced invalid expressions for better error
    // messages
    _expr_invalid: $ => choice(
      $.expr_invalid_return,
      $.expr_invalid_while,
      $.expr_invalid_return,
    ),

    expr_invalid_return: $ => seq(
      'return',
       optional($._expression)
    ),

    expr_invalid_while: $ => seq(
      'while',
      parens($, $._expression),
      $._expression,
    ),

    expr_invalid_for: $ => seq(
      'for',
      parens($, sep($._expression, ";")),
      $._expression,
    ),


    _expr_variable: $ => prec('EXPR_VAR', choice(
      $.identifier,
      $.qual_identifier
    )),

    expr_literal: $ => prec('EXPR_ATOM', field("literal", $._literal)),

    expr_record: $ => prec('EXPR_ATOM', braces_comma1($,
      field("field", $.expr_record_field)
    )),

    expr_record_field: $ => seq(
      field("name", $.identifier), '=',
      field("value", $._expression)
    ),

    expr_map: $ => prec('EXPR_ATOM', braces_comma1($,
      field("assign", $.map_assign)
    )),

    map_assign: $ => prec.left(seq(
      field("key", $.expr_map_key), '=',
      optional(seq('@', field("old_value", $.identifier))),
      field("new_value", $._expression)
    )),

    _expr_list: $ => choice(
      $.expr_list_literal,
      $.expr_list_range,
      $.expr_list_comprehension,
    ),

    expr_list_literal: $ => brackets_comma(
      $,
      field("elem", $._expression)
    ),

    expr_list_range: $ => brackets($,
      field("start", $._expression),
      '..',
      field("end", $._expression),
    ),

    expr_list_comprehension: $ => brackets(
      $,
      $._list_comprehension_yield,
      sep_comma1($, field("filter", $._list_comprehension_filter)),
    ),

    _list_comprehension_yield: $ => seq(
      field("yield", $._expression),
      '|',
    ),

    _list_comprehension_filter: $ => choice(
      $.list_comprehension_bind,
      alias($.expr_letval, $.list_comprehension_let),
      $.list_comprehension_if
    ),

    list_comprehension_bind: $ => seq(
      field("pattern", $._pattern),
      '<-',
      field("expr", $._expression)
    ),


    list_comprehension_if: $ => seq(
      'if', parens($, field("cond", $._expression)),
    ),

    expr_tuple: $ => prec('EXPR_TUPLE', parens_comma($,
      field("elem", $._expression)
    )),

    expr_hole: $ => prec('EXPR_ATOM', $._lex_hole),

    expr_wildcard: $ => prec('EXPR_ATOM', $._lex_wildcard),


    //**************************************************************************
    // PATTERN
    //**************************************************************************

    _pattern: $ => $._expression_simpl,

    pat_args: $ => alias($.expr_tuple, $.pat_args),

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
    ),

    lit_constructor: $ => $.qual_constructor,

    lit_bytes: $ => $._lex_bytes,

    lit_address: $ => $._lex_address,

    lit_lambda_op: $ => parens($, field("op", $._operator)),

    lit_integer: $ => choice(
      $._lex_int_dec,
      $._lex_int_hex,
    ),

    lit_bool: $ => choice('true', 'false'),

    lit_empty_map_or_record: $ => braces($),

    lit_string: $ => $._lex_string,

    lit_char: $ => $._lex_char,


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

    type_domain_zero: $ => parens($),

    type_domain_one: $ => prec('TYPE_DOMAIN', field("param", $._type)),

    type_domain_many: $ => parens_comma2($,
      field("param", $._type)
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

    type_params: $ => parens_comma($,
      field("param", $._type)
    ),

    _type_atom: $ => prec('TYPE_ATOM', choice(
      $.type_variable_poly,
      $.type_variable,
      $.type_paren,
    )),

    type_paren: $ => seq(
      '(',
      field("type", $._type),
      $._paren_close
    ),

    type_variable_poly: $ => $.type_variable_poly_name,

    type_variable: $ => choice($.identifier, $.qual_identifier),

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

    name: $ => choice($._lex_low_id, $._lex_up_id),

    type_variable_poly_name: $ => $._lex_prim_id,

    identifier: $ => $._lex_low_id,

    constructor: $ => $._lex_up_id,

    qual_identifier: $ => seq(
      field("path", $.qual),
      field("name", $.identifier),
    ),

    qual_constructor: $ => seq(
      field("path", $.qual),
      field("name", $.constructor),
    ),

    qual: $ => prec.left(repeat1(seq($.constructor, '.'))),


    _paren_close: $ => seq(optional($._inhibit_layout_end), ")"),
    _bracket_close: $ => seq(optional($._inhibit_layout_end), "]"),
    _brace_close: $ => seq(optional($._inhibit_layout_end), "}"),

    //**************************************************************************
    // LEXEMES
    //**************************************************************************

    _lex_int_dec: $ => token(/0|[1-9](_?[0-9]+)*/),
    _lex_int_hex: $ => token(/0x([0-9a-fA-F](_?[0-9a-fA-F]+)*)/),

    // This has to be above _lex_low_id in order to take priority. Somehow precedences don't work
    // here...
    _lex_address: $ => token(seq(
      choice('ak', 'ok', 'oq', 'ct'),
      /_[0-9a-zA-Z]+/,
    )),

    _lex_low_id: $ => token(/([a-z]|(_[a-zA-Z]))[a-zA-Z0-9_]*/),
    _lex_up_id: $ => token(/[A-Z][a-zA-Z0-9_]*/),

    _lex_prim_id: $ => token(/'*([a-z]|(_[a-zA-Z]))[a-zA-Z0-9_]*/),

    _lex_bytes: $ => token(/#[0-9a-fA-F]{2}(_?[0-9a-fA-F]{2})*/),

    _lex_wildcard: $ => token('_'),

    _lex_hole: $ => token('???'),

    _lex_string: $ => token(/"([^"\\]|(\\.))*"/),

    _lex_char: $ => token(/'(([\x00-\x26\x28-\x5b\x5d-\x7f])|([\x00-\xff][\x80-\xff]{1,3})|(\\[befnrtv'\\])|(\\x[0-9a-fA-F]{2,2})|(\\x\{[0-9a-fA-F]*\}))'/),

  }
});
