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


const OP_ASSOC = {
  'OP_PIPE': 'left',
  'OP_PIPE': 'left',
  'OP_TYPE': 'left',
  'OP_DISJ': 'right',
  'OP_CONJ': 'right',
  'OP_BOR': 'left',
  'OP_BXOR': 'left',
  'OP_BAND': 'left',
  'OP_CMP': 'left',
  'OP_LIST': 'right',
  'OP_SHIFT': 'left',
  'OP_ADD': 'left',
  'OP_NEG': 'unary',
  'OP_MUL': 'left',
  'OP_POW': 'left',
  'OP_NOT': 'unary',
};


module.exports = grammar({
  name: 'aesophia',


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
    $._inhibit_keyword_termination,
    // @ts-ignore: DSL not updated for literals
    ",",
    $._synchronize,
    $._invalid_layout,
    $._prefix_operator,
    $._symbol_export_marker,
  ],


  extras: $ => [
    /[\n\r ]+/,
    $._synchronize,
    $.line_comment,
    $.block_comment,
    $.doc_comment,
  ],


  inline: $ => [
    $.identifier,
    $.constructor,
    $.field_name,
    $.scope_name,
    $.qual_constructor,
    $.qual_identifier,
    $.qual_scope_name,
    $._expr_list,
    $.pattern,
    $.pat_args,
  ],


  conflicts: $ => [
    // Modifiers screw them up
    [$.function_declaration, $.scope_declaration,]
  ],


  precedences: $ => [
    [ // expressions
      'EXPR_LETVAL',
      'EXPR_APP',
      $.expr_op,
      $._expression_body,
      $.expr_typed,
      'EXPR_INVALID',
    ],


    [ // operators
      'OP_NOT',
      'OP_POW',
      'OP_MUL',
      'OP_NEG',
      'OP_ADD',
      'OP_SHIFT',
      'OP_LIST',
      'OP_CMP',
      'OP_BAND',
      'OP_BXOR',
      'OP_BOR',
      'OP_CONJ',
      'OP_DISJ',
      'OP_PIPE',
    ],

    [ // types
      // $.type_application, $.type_tuple, $._type_in_tuple,
      $.type_domain, $.type_function,
    ],

    [
      $.type_application, $._type_in_tuple,
    ],

    [
      $.type_tuple, $._type,
    ],


    // (x)
    // this is somehow unclear whether it's a tuple or a beginning of a lambda
    [
      $._expression_simpl, $.expr_lambda,
    ],

    // switch(x) p | x : type => ...
    //               ^Should be parsed as typed guard, not x : function
    [
      $.expr_typed, $.type_domain,
    ],

    [
      $.argument_name, $.qual_low_id, $._expr_match,
    ],

    [
      $._expression, $._expr_invalid_for_content,
    ],
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
      dispatch($, 'decl', $._decl),
      dispatch($, 'type', $._type),
      dispatch($, 'literal', $._literal),
      dispatch($, 'expression', $._expression),
      dispatch($, 'scope_declaration', $.scope_declaration),
      dispatch($, 'function_declaration', $.function_declaration),
      dispatch($, '_scoped_declaration', $._scoped_declaration),
    ),


    //**************************************************************************
    // COMMENTS
    //**************************************************************************

    line_comment: $ => seq(
      $._line_comment_start,
      alias($.comment_content, $.line_comment),
    ),

    block_comment: $ => seq(
      $._block_comment_start,
      alias($._block_comment_content, $.block_comment),
      $._block_comment_end,
    ),

    doc_comment: $ => seq(
      $._doc_comment_start,
      alias($._doc_comment_content, $.doc_comment),
      $._block_comment_end,
    ),

    _line_comment_start: $ => token('//'),
    _block_comment_start: $ => token('/*'),
    _block_comment_end: $ => token('*/'),
    _doc_comment_start: $ => token('/**'),


    //**************************************************************************
    // TOP LEVEL
    //**************************************************************************

    module: $ => repeat1(seq($._decl, $._layout_terminator)),

    _decl: $ => choice(
      $.pragma,
      $.include,
      $.using,
      $.scope_declaration,
      $._scoped_declaration,
    ),


    pragma: $ => seq(
      '@',
      field("pragma", choice(
        $.pragma_compiler_vsn
      )),
    ),

    pragma_compiler_vsn: $ => seq(
      token.immediate('compiler'),
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
      field("scope", $.scope_name),
      optional(field("as", $.using_as)),
      optional(field("select", $._using_select))
    ),

    using_as: $ => seq(
      'as',
      field("alias", $.name)
    ),

    _using_select: $ => choice(
      $.using_hiding,
      $.using_for,
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
      field("modifier", repeat($.modifier)),
      field("head", $.scope_head),
      field("modifier", alias(optional('interface'), $.interface)),
      field("name", $.scope_name),
      optional($._implemented_interfaces),
      '=',
      maybe_block($, field("decl", $._decl))
    ),

    _implemented_interfaces: $ => seq(
      ':',
      sep1(field("implements", $.qual_scope_name), ",")
    ),

    scope_head: $ => choice(
      'contract', 'namespace'
    ),


    _scoped_declaration: $ => choice(
      $._type_definition,
      $.function_declaration,
      alias($._letval, $.letval),
    ),


    _letval: $ => seq(
      'let',
      field("pattern", $.pattern),
      '=',
      field("value", $._expression_body),
    ),


    //**************************************************************************
    // FUNCTION DECLARATION
    //**************************************************************************

    function_declaration: $ => seq(
      field("modifiers", repeat(field("modifier", $.modifier))),
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
      field("body", $._function_body),
    ),

    _function_body: $ => choice(
      $.unguarded_body,
      $.guarded_bodies,
    ),

    unguarded_body: $ => seq(
      '=',
      field("body", $._expression_body),
    ),

    guarded_bodies: $ => prec.right(repeat1(seq(
      '|', field("branch", $.guarded_body)
    ))),

    guarded_body: $ => seq(
      sep1(field("guard", $._expr_guard), ','), '=',
      field("body", $._expression_body)
    ),


    function_head: $ => choice(
      'function',
      'entrypoint'
    ),

    modifier: $ => choice(
      'main',
      'payable',
      'stateful',
      'private',
      'public',
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
      optional($._type_alias_body),
    ),

    _type_alias_body: $ => seq(
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


    /// Expressions that might be indented blocks
    _expression_body: $ => choice(
      $.expr_block,
      $._expression,
    ),

    expr_block: $ => block($, $._expression),


    /// Expressions that are not indented blocks
    _expression: $ => choice(
      $._expression_complex,
      $._expression_simpl,
      $._expr_invalid,
    ),

    /// Things that may contain unwrapped indented blocks
    _expression_complex: $ => choice(
      $.expr_lambda,
      $.expr_if,
      alias($.using, $.expr_using),
      $.expr_letval,
      $.expr_switch,
    ),

    /// Things that have unambiguous beginning and end
    _expression_simpl: $ => choice(
      $.expr_typed,
      $.expr_op,
      $.expr_application,
      $.expr_map_or_record,
      $.expr_map_access,
      $.expr_projection,
      $.expr_tuple,
      alias($._expr_match, $.expr_match),
      $._expr_atom,
    ),

    _expr_atom: $ => choice(
      $.expr_literal,
      $.expr_variable,
      $._expr_list,
      $.expr_hole,
      $.expr_wildcard,
    ),


    /// (x, y) => x + y
    expr_lambda: $ => prec.right(seq(
      field("args", $.pat_args),
      '=>',
      field("body", $._expression_body)
    )),


    /// x + y
    expr_op: $ => choice(
      ...[
        [$.op_pipe, 'OP_PIPE'],
        [$.op_or,   'OP_DISJ'],
        [$.op_and,  'OP_CONJ'],
        [$.op_bor,  'OP_BOR'],
        [$.op_bxor, 'OP_BXOR'],
        [$.op_band, 'OP_BAND'],
        [$.op_lt,   'OP_CMP'],
        [$.op_gt,   'OP_CMP'],
        [$.op_le,   'OP_CMP'],
        [$.op_ge,   'OP_CMP'],
        [$.op_eq,   'OP_CMP'],
        [$.op_neq,  'OP_CMP'],
        [$.op_cons, 'OP_LIST'],
        [$.op_cat,  'OP_LIST'],
        [$.op_bsl,  'OP_SHIFT'],
        [$.op_bsr,  'OP_SHIFT'],
        [$.op_add,  'OP_ADD'],
        [$.op_sub,  'OP_ADD'],
        [$.op_sub,  'OP_NEG'],
        [$.op_mul,  'OP_MUL'],
        [$.op_div,  'OP_MUL'],
        [$.op_mod,  'OP_MUL'],
        [$.op_pow,  'OP_POW'],
        [$.op_bnot, 'OP_NOT'],
        [$.op_not,  'OP_NOT'],
      ].map(([operator, precedence]) =>
        (OP_ASSOC[precedence] === 'left' ? prec.left :
         OP_ASSOC[precedence] === 'right' ? prec.right :
         prec
        )(
          precedence,
          OP_ASSOC[precedence] === 'unary' ? seq(
            field("op", operator),
            field("op_r", $._expression_simpl),
          ) : seq(
            field("op_l", $._expression_simpl),
            field("op", operator),
            field("op_r", $._expression_simpl),
          )
        )
      )
    ),


    /// x : int
    expr_typed: $ => prec.left(seq(
      field("expr", $._expression_simpl), ':',
      field("type", $._type)
    )),


    /// f(x, y)
    expr_application: $ => prec.left('EXPR_APP', seq(
      field("fun", $._expression_simpl),
      field("args", $.expr_args),
    )),

    expr_args: $ => parens_comma($,
      field("arg", $.expr_argument)
    ),

    expr_argument: $ => seq(
      optional(seq(field("name", $.argument_name), '=')),
      field("value", $._expression)
    ),

    argument_name: $ => alias($.identifier, $._),


    /// Maps and records are basically the same thing for the parser.
    /// - {}
    /// - {f = 3}
    /// - {[k] = 3}
    /// - r{f.g[k] = 2}
    /// - m{[k] = 3}
    expr_map_or_record: $ => prec.left('EXPR_APP', seq(
      optional(field("source", $._expression_simpl)),
      field("updates", $.member_assigns),
    )),

    member_assigns: $ => braces_comma(
      $,
      field("update", $.member_assign)
    ),

    member_assign: $ => seq(
      field("path_head", $._member_assign_path_head),
      optional(field("path", $.member_assign_path)),
      optional(field("old_value", $._default_value)),
      '=',
      field("new_value", $._expression)
    ),

    _member_assign_path_head: $ => choice(
      $.field_name,
      $.map_key,
    ),

    _member_assign_path_elem: $ => choice(
      seq('.', $.field_name),
      $.map_key,
    ),

    member_assign_path: $ => seq(
      repeat1($._member_assign_path_elem),
    ),

    _default_value: $ => seq(
      '@',
      $.identifier,
    ),


    /// m[k]
    expr_map_access: $ => prec.left('EXPR_APP', seq(
      field("map", $._expression_simpl),
      field("key", $.map_key)
    )),

    map_key: $ => brackets(
      $,
      field("key", $._expression_simpl),
      optional(field("default", $._map_default_value)),
    ),

    _map_default_value: $ => seq(
      '=',
      $._expression
    ),


    /// r.f
    expr_projection: $ => prec.left('EXPR_APP', seq(
      field("expr", $._expression_simpl), '.',
      field("field", $.identifier)
    )),


    /// if(x)
    ///   if(y) 3
    /// else
    ///   2
    expr_if: $ => prec.right(seq(
      $._if_head,
      field("then", $._expression_body),
      repeat(
        choice(
          $._inhibit_keyword_termination,
          $.expr_elif
        )
      ),
      optional($.expr_else),
    )),

    _if_head: $ => seq(
      'if',
      parens($, field("cond", $._expression)),
    ),

    _if_branch: $ => choice($.expr_elif, $.expr_else),

    expr_elif: $ => seq(
      'elif',
      parens($, field("cond", $._expression)),
      field("then", $._expression_body)
    ),

    expr_else: $ => seq(
      'else',
      field("then", $._expression_body)
    ),


    /// switch(x) y => y
    expr_switch: $ => seq(
      'switch',
      parens($, field("expr", $._expression)),
      field("cases", $.expr_cases)
   ),

    expr_cases: $ => maybe_block($, field("case", $.switch_case)),

    switch_case: $ => seq(
      field("pattern", $.pattern),
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
      sep1(field("guard", $._expr_guard), ','), '=>',
      field("body", $._expression_body)
    ),

    _expr_guard: $ => $._expression,


    /// We allow meaningless letval expressions to relief the parser. It's the AST builder which
    /// will check if that's a statement
    /// let x = 2
    expr_letval: $ => prec('EXPR_LETVAL', $._letval),


    /// Used in patterns
    /// x = y
    _expr_match: $ => prec.right(seq(
      field("pattern", $.identifier),
      '=',
      field("value", $.pattern)
    )),


    _expr_list: $ => choice(
      $.expr_list_literal,
      $.expr_list_range,
      $.expr_list_comprehension,
    ),


    /// [1, 2, 3]
    expr_list_literal: $ => brackets_comma(
      $,
      field("elem", $._expression)
    ),


    /// [1 .. 2]
    expr_list_range: $ => brackets($,
      field("start", $._expression),
      '..',
      field("end", $._expression),
    ),


    /// [ x | x <- l ]
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
      $.list_comprehension_let,
      $.list_comprehension_if,
    ),

    list_comprehension_let: $ => $._letval,

    list_comprehension_bind: $ => seq(
      // field("pattern", $.expr_variable),
      field("pattern", $.pattern),
      '<-',
      field("expr", $._expression)
    ),

    list_comprehension_if: $ => $._if_head,


    /// (1, 2, x)
    expr_tuple: $ => parens_comma($,
      field("elem", $._expression)
    ),


    /// x
    expr_variable: $ => $.qual_identifier,


    /// 1
    expr_literal: $ =>field("literal", $._literal),


    /// ???
    expr_hole: $ => $._lex_hole,


    /// _
    expr_wildcard: $ => $._lex_wildcard,


    // We allow tree sitter to parse commonly misplaced invalid expressions for better error
    // messages
    _expr_invalid: $ => choice(
      $.expr_invalid_return,
      $.expr_invalid_while,
      $.expr_invalid_for,
    ),

    expr_invalid_return: $ => prec.right('EXPR_INVALID', seq(
      'return',
       optional($._expression_simpl)
    )),

    expr_invalid_while: $ => prec.right('EXPR_INVALID', seq(
      'while',
      parens($, $._expression),
      $._expression,
    )),

    expr_invalid_for: $ => prec.right('EXPR_INVALID', seq(
      'for',
      $._expr_invalid_for_content,
    )),

    _expr_invalid_for_content: $ => prec.right('EXPR_INVALID', choice(
      'in', ';', $._expression_simpl, parens($, $._expr_invalid_for_content),
    )),


    //**************************************************************************
    // PATTERN
    //**************************************************************************

    pattern: $ => alias($._expression_simpl, $.pattern),

    pat_args: $ => alias($.expr_tuple, $.pat_args),


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
    op_bor: $ => 'bor',
    op_bxor: $ => 'bxor',
    op_band: $ => 'band',
    op_lt: $ => '<',
    op_gt: $ => '>',
    op_le: $ => '=<',
    op_ge: $ => '>=',
    op_eq: $ => '==',
    op_neq: $ => '!=',
    op_cons: $ => '::',
    op_cat: $ => '++',
    op_bsl: $ => '<<',
    op_bsr: $ => '>>',
    op_add: $ => '+',
    op_sub: $ => '-',
    op_mul: $ => '*',
    op_div: $ => '/',
    op_mod: $ => 'mod',
    op_pow: $ => '^',
    op_bnot: $ => 'bnot',
    op_not: $ => '!',

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

    lit_string: $ => $._lex_string,

    lit_char: $ => $._lex_char,


    //**************************************************************************
    // TYPE
    //**************************************************************************

    /// Only for constructors
    type_indexed: $ => seq(
      'indexed',
      $._type,
    ),

    _type: $ => choice(
      $.type_tuple,
      $.type_function,
      $._type_in_tuple,
    ),

    /// Tuple and function types are open and unprecedenced. This confuses the parses, thus we need
    /// to separate types that must not be tuple members without parens.
    _type_in_tuple: $ => choice(
      $.type_application,
      $.type_variable_poly,
      $.type_variable,
      $.type_contract,
      $.type_paren,
      $.type_wildcard,
    ),


    /// int => int
    type_function: $ => prec.right(seq(
      $.type_domain,
      '=>',
      field("codomain", $._type)
    )),

    type_domain: $ => choice(
      parens_comma2($, $._type),
      seq('(', $._paren_close),
      $._type,
    ),


    /// int * int * int
    type_tuple: $ => prec.right(seq(
      $._type_in_tuple,
      '*',
      $._type_tuple,
    )),

    _type_tuple: $ => prec.right(seq(
      $._type_in_tuple,
      optional(seq('*', $._type_tuple)),
    )),


    /// list(int)
    type_application: $ => prec.left(seq(
      field("fun", $.type_variable),
      field("params", $.type_params),
    )),

    type_params: $ => choice(
      parens_comma($, field("param", $._type_param))
    ),

    _type_param: $ => choice(
      alias($.lit_integer, $.type_integer),
      $.type_indexed,
      $._type,
    ),


    /// (int)
    type_paren: $ => prec.right(seq(
      '(',
      field("type", $._type),
      $._paren_close
    )),


    /// 'a
    type_variable_poly: $ => $.type_variable_poly_name,

    /// int
    type_variable: $ => $.qual_identifier,

    /// Auction
    type_contract: $ => $.qual_scope_name,

    /// _
    type_wildcard: $ => $._lex_wildcard,


    //**************************************************************************
    // NAMES
    //**************************************************************************

    name: $ => choice($._lex_low_id, $._lex_up_id),

    type_variable_poly_name: $ => $._lex_prim_id,

    identifier: $ => alias($._lex_low_id, $.identifier),

    constructor: $ => alias($._lex_up_id, $.constructor),

    field_name: $ => alias($._lex_low_id, $.field_name),

    scope_name: $ => alias($._lex_up_id, $.scope_name),

    qual_low_id: $ => seq(
      field("path", repeat(seq($.scope_name, token.immediate('.')))),
      alias($._lex_low_id, $.name)
    ),

    qual_up_id: $ => seq(
      field("path", repeat(seq($.scope_name, token.immediate('.')))),
      alias($._lex_up_id, $.name)
    ),

    qual_identifier: $ => $.qual_low_id,

    qual_constructor: $ => $.qual_up_id,

    qual_scope_name: $ => $.qual_up_id,

    qual: $ => repeat1(seq(field("path", $.scope_name), '.')),


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

    _lex_low_id: $ => token(/([a-z_]|(_[a-zA-Z]))[a-zA-Z0-9_']*/),
    _lex_up_id: $ => token(/[A-Z][a-zA-Z0-9_']*/),

    _lex_prim_id: $ => token(/'*([a-z]|(_[a-zA-Z]))[a-zA-Z0-9_]*/),

    _lex_bytes: $ => token(/#[0-9a-fA-F](_?[0-9a-fA-F])*/),

    _lex_wildcard: $ => token('_'),

    _lex_hole: $ => token('???'),

    _lex_string: $ => token(/"([^"\\]|(\\.))*"/),

    _lex_char: $ => token(/'(([\x00-\x26\x28-\x5b\x5d-\x7f])|([\x00-\xff][\x80-\xff]{1,3})|(\\[befnrtv'\\])|(\\x[0-9a-fA-F]{2,2})|(\\x\{[0-9a-fA-F]*\}))'/),

  }
});
