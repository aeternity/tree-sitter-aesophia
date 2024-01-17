const {
  parens, brackets, braces,
  wrap_comma, wrap_comma1, wrap_comma2,
  parens_comma, parens_comma1, parens_comma2,
  braces_comma, braces_comma1, braces_comma2,
  brackets_comma, brackets_comma1, brackets_comma2,
  block, block_or, maybe_block, weak_block, inline_block,
  dispatch,
  sep, sep1, sep2,
  qual, qual1,
} = require('./grammar_utils.js');

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
    [$.pat_args, $.expr_tuple],
  ],

  extras: $ => [
    $.doc_comment,
    $.block_comment,
    $.line_comment,
    /[\s\uFEFF\u2060\u200B]/,
  ],

  externals: $ => [
    $._block_open_inline,
    $._block_open,
    $._block_semi,
    $._block_close,
    $._block_comment_content,
    $._error_state,
  ],

  supertypes: $ => [
    $._top_decl,
    $._type_definition,
    $._scoped_declaration,
    $._expression,
    $._list_comprehension_filter,
    $._literal,
    $._statement,
    $._type,
    $._operator,
  ],

  inline: $ => [
    $._expression_body,
    $._expr_atom,
    $._pattern,
    $.type_variable_poly_name,
    $.using_as,
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

      'EXPR_IF',
      'EXPR_SWITCH',
      'EXPR_GUARD',
      'EXPR_LAMBDA',
    ],

    [ // statements
      'STMT_EXPR',
      'STMT_IF',
      'STMT_LET',
    ],

    [ // types
      'TYPE_ATOM',
      'TYPE_QUAL',
      'TYPE_APP',
      'TYPE_DOMAIN',
      'TYPE_TUPLE',
      'TYPE_FUN',
    ],

    [ // Literals
      'LIT_ATOM',
      'LIT_QUAL',
    ],

    // conflicts

    // switch(x) p | x : type => ...
    //               ^Should be parsed as typed guard
    [ 'EXPR_TYPED', 'TYPE_DOMAIN' ],
  ],

  word: $ => $._lex_low_id,

  rules: {
    source: $ => choice(
      seq($._dispatch, repeat('\n')),
      field("module", $.module)
    ),

    _dispatch: $ => choice(
      dispatch($, 'module', $.module),
      dispatch($, 'top_decl', $._top_decl),
      dispatch($, 'type', $._type),
      dispatch($, 'literal', $._literal),
      dispatch($, 'expression', $._expression),
      dispatch($, 'statement', $._statement),
      dispatch($, 'statements', maybe_block($, $._statement)),
      dispatch($, 'scope_declaration', $.scope_declaration),
      dispatch($, '_scoped_declaration', $._scoped_declaration),
    ),

    module: $ => repeat1(seq($._top_decl, $._block_semi)),

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

    using_name_list: $ => brackets_comma1(
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
      optional(seq(":", sep1(field("implements", $.qual_constructor_name), ","))),
      '=',
      maybe_block($, field("decl", $._scoped_declaration))
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
      weak_block($, field("clause",
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

    type_param_decls: $ => parens_comma(
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

    record_fields: $ => braces_comma(
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

    _expression_body: $ => choice(
      $.expr_block,
      $._expression,
    ),

    _expression: $ => choice(
      $.expr_lambda,
      $.expr_if,
      $.expr_variable,
      $.expr_typed,
      $.expr_match,
      $.expr_switch,
      $.expr_op,
      $.expr_application,
      $.expr_record_update,
      $.expr_map_update,
      $.expr_map_access,
      $.expr_projection,
      $.expr_tuple,
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
            field("op_r", $._expression)
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

    expr_args: $ => parens_comma(
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

    record_field_updates: $ => braces_comma1(
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

    map_updates: $ => braces_comma1(
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

    expr_map_key: $ => brackets(
      field("key", $._expression),
      optional(seq('=', field("default_value", $._expression))),
    ),

    expr_projection: $ => prec.left('EXPR_PROJECTION', seq(
      field("expr", $._expression), '.',
      field("field", $.identifier)
    )),

    expr_if: $ => prec('EXPR_IF', seq(
      'if',
      parens(field("cond", $._expression)),
      field("then", $._expression),
      repeat($._expr_elif),
      $._expr_else
    )),

    _expr_elif: $ => prec('EXPR_IF', seq(
      'elif',
      parens(field("cond", $._expression)),
      field("then", $._expression),
    )),

    _expr_else: $ => prec('EXPR_IF', seq(
      'else',
      field("else", $._expression)
    )),

    expr_switch: $ => prec('EXPR_SWITCH', seq(
      'switch',
      parens(field("expr", $._expression)),
      field("cases", $.expr_cases)
    )),

    expr_cases: $ => weak_block($, field("case", $.switch_case)),

    switch_case: $ => seq(
      field("pattern", $._pattern),
      field("branch", $._switch_branch)
    ),

    _switch_branch: $ => prec.right(choice(
      $.unguarded_branch,
      $.guarded_branches,
    )),

    unguarded_branch: $ => prec('EXPR_SWITCH', seq(
      $._block_open_inline,
      '=>',
      field("body", $._expression_body),
      $._block_close
    )),

    guarded_branches: $ => prec('EXPR_GUARD', weak_block($, seq(
      '|', field("branch", $.guarded_branch)
    ))),

    guarded_branch: $ => prec('EXPR_GUARD', seq(
      sep1(field("guard", $._expression), ','), '=>',
      field("body", $._expression_body)
    )),

    expr_block: $ => prec('EXPR_BLOCK', block(
      $, field("stmt", $._statement)
    )),

    expr_variable: $ => prec('EXPR_VAR', $.identifier),

    _expr_atom: $ => prec('EXPR_ATOM', choice(
      $.expr_literal,
      $.expr_record,
      $.expr_map,
      $._expr_list,
      $.expr_hole,
      $.expr_wildcard,
    )),

    expr_literal: $ => prec('EXPR_ATOM', field("literal", $._literal)),

    expr_record: $ => prec('EXPR_ATOM', braces_comma1(
      field("field", $.expr_record_field)
    )),

    expr_record_field: $ => seq(
      field("name", $.identifier), '=',
      field("value", $._expression)
    ),

    expr_map: $ => prec('EXPR_ATOM', braces_comma1(
      field("assign", $.map_assign)
    )),

    map_assign: $ => seq(
      field("key", $.expr_map_key), '=',
      optional(seq('@', field("old_value", $.identifier))),
      field("new_value", $._expression)
    ),

    _expr_list: $ => choice(
      $.expr_list_literal,
      $.expr_list_range,
      $.expr_list_comprehension
    ),

    expr_list_literal: $ => prec('EXPR_ATOM', brackets_comma(
      field("elem", $._expression)
    )),

    expr_list_range: $ => prec('EXPR_ATOM', brackets(
      field("start", $._expression), '..',
      field("end", $._expression),
    )),

    expr_list_comprehension: $ => prec('EXPR_ATOM', seq(
      '[',
      field("yield", $._expression),
      wrap_comma1(
        '|',
        field("filter", $._list_comprehension_filter),
        ']'
      ),
    )),

    _list_comprehension_filter: $ => choice(
      $.list_comprehension_bind,
      $.list_comprehension_let,
      $.list_comprehension_if
    ),

    list_comprehension_bind: $ => seq(
      field("pattern", $._pattern), '<-',
      field("expr", $._expression)
    ),

    list_comprehension_let: $ => seq(
      'let',
      field("pattern", $._pattern), '=',
      field("expr", $._expression)
    ),

    list_comprehension_if: $ => seq(
      'if', parens(field("cond", $._expression)),
    ),

    expr_tuple: $ => prec('EXPR_TUPLE', parens_comma(
      field("elem", $._expression)
    )),

    expr_hole: $ => prec('EXPR_ATOM', $._lex_hole),

    expr_wildcard: $ => prec('EXPR_ATOM', $._lex_wildcard),

    expr_match: $ => prec.right('EXPR_MATCH', seq(
      field("lvalue", $._expression),
      '=',
      field("rvalue", $._expression)
    )),

    //**************************************************************************
    // PATTERN
    //**************************************************************************

    _pattern: $ => $._expression,

    pat_args: $ => parens_comma(
      field("arg", $._pattern)
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
    ),

    lit_constructor: $ => prec.left('LIT_QUAL', $.qual_constructor_name),

    lit_bytes: $ => prec('LIT_ATOM', $._lex_bytes),

    lit_address: $ => prec('LIT_ATOM', $._lex_address),

    lit_lambda_op: $ => prec('LIT_ATOM', parens(field("op", $._operator))),

    lit_integer: $ => prec('LIT_ATOM', choice(
      $._lex_int_dec,
      $._lex_int_hex,
    )),

    lit_bool: $ => choice('true', 'false'),

    lit_empty_map_or_record: $ => braces(),

    lit_string: $ => $._lex_string,

    lit_char: $ => $._lex_char,

    //**************************************************************************
    // STATEMENT
    //**************************************************************************

    _statement: $ => choice(
      $.stmt_letval,
      // $.stmt_letfun,
      $.stmt_if,
      $.stmt_elif,
      $.stmt_else,
      $.stmt_expr,
      $.stmt_invalid,
    ),

    stmt_letval: $ => prec('STMT_LET', seq(
      'let',
      field("pattern", $._pattern),
      '=',
      field("value", $._expression)
    )),

    // stmt_letfun: $ => prec('STMT_LET', seq(
    //   'let',
    //   field("name", $.identifier),
    //   field("args", $.pat_args),
    //   optional(seq(':', field("type", $._type))), '=',
    //   field("value", $._expression)
    // )),

    stmt_if: $ => prec.right('STMT_IF', seq(
      'if',
      parens(field("cond", $._expression)),
      field("then", $._expression_body),
    )),

    stmt_elif: $ => prec('STMT_IF', seq(
      'elif',
      parens(field("cond", $._expression)),
      field("then", $._expression_body)
    )),

    stmt_else: $ => prec.right('STMT_IF', seq(
      'else',
      field("else", $._expression_body),
    )),

    stmt_invalid: $ => choice(
      $.stmt_invalid_return,
      $.stmt_invalid_while,
      $.stmt_invalid_return,
    ),

    stmt_invalid_return: $ => seq(
      'return',
       optional($._expression)
    ),

    stmt_invalid_while: $ => seq(
      'while',
      parens($._expression),
      $._expression,
    ),

    stmt_invalid_for: $ => seq(
      'for',
      parens(sep($._expression, ";")),
      $._expression,
    ),

    // In statements, all ifs should have their branches indented the same way.  Otherwise, a single
    // space can have vast effects on the AST structure. Consider examples:
    //
    // f() =
    //   if(x)
    //      if(x)    1
    //      elif(x)  2
    //   elif(x)     3
    //   else        4
    //
    // f() =
    //   if(x)
    //      if(x)    1
    //       elif(x) 2
    //   elif(x)     3
    //   else        4
    //
    // In the first one compiles to a 3-way if statement (2-way if statement (1; 2); 3; 4), while
    // the second is a 1-way if statement with a 4-way if *expression* (1;2;3;4).  This however
    // should be checked by the linter and we allow parsing both. Uncomment the following to ban
    // this behavior: stmt_expr: $ => prec('STMT_EXPR', $._expression_non_if),
    stmt_expr: $ => prec('STMT_EXPR', field("expr", $._expression)),

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

    type_domain_zero: $ => parens(),

    type_domain_one: $ => prec('TYPE_DOMAIN', field("param", $._type)),

    type_domain_many: $ => parens_comma2(
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

    type_params: $ => parens_comma(
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
      ')'
    ),

    type_variable_poly: $ => $.type_variable_poly_name,

    type_variable: $ => $.qual_identifier_name,

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

    qual_identifier_name: $ => seq(
      field("path", $.qual),
      field("name", $.identifier),
    ),

    qual_constructor_name: $ => seq(
      field("path", $.qual),
      field("name", $.constructor),
    ),

    qual: $ => prec.left(repeat1(seq($.constructor, '.'))),

    //**************************************************************************
    // LEXEMES
    //**************************************************************************

    block_open: $ => $._block_open,
    block_open_inline: $ => $._block_open_inline,
    block_semi: $ => $._block_semi,
    block_close: $ => $._block_close,

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

    doc_comment: ($) =>
    seq("/**", $._block_comment_content, "*/"),

    block_comment: ($) =>
    seq("/*", $._block_comment_content, "*/"),

    line_comment: ($) => token(seq(/\/\//, repeat(/[^\n]/))),
  }
});
