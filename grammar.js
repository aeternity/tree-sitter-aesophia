const field_miss = ($, pred, name, rule) => seq(pred, choice(
  field(name, rule),
  alias($._block_empty, $['MISSING_' + name]),
));

const miss = ($, pred, name, rule) => seq(pred, choice(
  rule,
  alias($._block_empty, $['MISSING_' + name]),
));

const sub = ($, rule) => seq(
  $.vopen,
  rule,
  $.vclose,
);

const block = ($, rule) =>
      sub($, repeat1(seq(rule, $.vsemi)));

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


  externals: $ => [
    $._block_comment_content,
    $._doc_comment_content,
    $.comment_content,
    $._vopen,
    $._vclose,
    $._vsemi,
    $.invalid_indent,
    $._indent_at_level,
    $._indent_not_at_level,
    $._block_empty,
    $._inhibit_vclose,
    $._inhibit_vsemi,
    ",",
    "|",
    $._synchronize,
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
    $.scope_name,
    $.qual_constructor,
    $.qual_identifier,
    $.qual_scope_name,
    $.expr_literal,
    $.pat_literal,
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


    [ // patterns
      $.pat_variable,
      'PAT_APP',
      $.pat_op,
      $.pat_match,
      $.pat_typed,
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


    // switch(x) p | x : type => ...
    //               ^Should be parsed as typed guard, not x : function
    [
      $.expr_typed, $.pat_typed, $.type_domain,
    ],

    // (x, y) =>
    // This is a lambda obviously
    [
      $.expr_tuple, $.pat_args, $.pat_tuple,
    ],


    // INVALID stuff can be quite arbitrary
    [
      $._expression, $._INVALID_expr_content,
    ],
  ],


  conflicts: $ => [
    // Modifiers screw them up
    [$.function_declaration, $.scope_declaration,],

    [$.pat_variable, $.qual_low_id],
    [$._expression_simple, $._pattern],
    [$.pat_list, $.expr_list_literal],
    [$.pat_args, $.pat_tuple],
    [$.pat_match, $._old_value],
    [$.member_assigns, $.pat_map_or_record],
  ],


  supertypes: $ => [
    $._decl,
    $._using_select,
    $._scoped_declaration,
    $._type_definition,
    $._expression,
    $._expression_body,
    $._expression,
    $._expression_simple,
    $._INVALID_expr,
    $._list_comprehension_filter,
    $._pattern,
    $._operator,
    $._literal,
    $._type,
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

    module: $ => repeat1(seq(field("decl", $._decl), $.vsemi)),

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
      field("scope", $.qual_scope_name),
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
      field("modifier", optional(alias('interface', $.interface))),
      field("name", $.scope_name),
      optional($._implemented_interfaces),
      miss($, '=', "decls",
           maybe_block($, field("decl", $._decl))),
    ),

    _implemented_interfaces: $ =>
      miss($, ':', "interfaces", sep1(field("implements", $.qual_scope_name), ","))
    ,

    scope_head: $ => choice(
      'contract', 'namespace'
    ),


    _scoped_declaration: $ => choice(
      $._type_definition,
      $.function_declaration,
      alias($._letval, $.letval),
    ),


    _letval: $ => prec.right(seq(
      'let',
      field("pattern", $._pattern),
      '=',
      field("value", $._expression_body),
    )),


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
      field("name", $.identifier),
      field_miss($, ':', "type", $._type)
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

    unguarded_body: $ =>
    field_miss($, '=', "body", $._expression_body),

    guarded_bodies: $ => prec.right(repeat1(
      field("branch", $.guarded_body)
    )),

    guarded_body: $ => seq(
      '|',
      sep1(field("guard", $._expression), ','),
      '=',
      field("body", $._expression_body)
    ),

    function_head: $ => choice(
      'function',
      'entrypoint',
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

    type_param_decls: $ => parens_comma(
      $,
      field("name", $.type_param)
    ),

    type_param: $ => $.type_variable_poly_name,


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
      repeat($._inhibit_vsemi),
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
      miss($, '=', "constructors", sep1(field("constructor", $.constructor_declaration), '|')),
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

    expr_block: $ => block($, field("stmt", $._expression)),


    /// Expressions that are not indented blocks
    _expression: $ => choice(
      $.expr_lambda,
      $.expr_if,
      $.expr_letval,
      $.expr_switch,
      $._expression_simple,
    ),

    /// Things that have unambiguous beginning and end
    _expression_simple: $ => choice(
      alias($.using, $.expr_using),
      $.expr_typed,
      $.expr_op,
      $.expr_application,
      $.expr_map_or_record,
      $.expr_map_access,
      $.expr_projection,
      $.expr_tuple,
      $.expr_literal,
      $.expr_variable,
      $._expr_list,
      alias($._lex_hole, $.expr_hole),
      $._INVALID_expr,
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
            field("op_r", $._expression_simple),
          ) : seq(
            field("op_l", $._expression_simple),
            field("op", operator),
            field("op_r", $._expression_simple),
          )
        )
      )
    ),


    /// x : int
    expr_typed: $ => prec.left(seq(
      field("expr", $._expression_simple), ':',
      field("type", $._type)
    )),


    /// f(x, y)
    expr_application: $ => prec.left('EXPR_APP', seq(
      field("fun", $._expression_simple),
      field("args", $.expr_args),
    )),

    expr_args: $ => parens_comma($,
      field("arg", $.expr_arg)
    ),

    expr_arg: $ => seq(
      optional(seq(field("name", $.identifier), '=')),
      field("value", $._expression)
    ),


    /// Maps and records are basically the same thing for the parser.
    /// - {}
    /// - {f = 3}
    /// - {[k] = 3}
    /// - r{f.g[k] = 2}
    /// - m{[k] = 3}
    expr_map_or_record: $ => prec.left('EXPR_APP', seq(
      optional(field("source", $._expression_simple)),
      field("assigns", $.member_assigns),
    )),

    member_assigns: $ => braces_comma(
      $,
      field("assign", $.member_assign)
    ),

    member_assign: $ => seq(
      optional(field("path", $.member_path)),
      optional($._old_value),
      '=',
      field("new_value", $._expression)
    ),

    member_path: $ => seq(
      field("member", $._member_path_head),
      repeat($._member_path_elem),
    ),

    _member_path_head: $ => choice(
      $.identifier,
      $.map_key,
    ),

    _member_path_elem: $ => choice(
      seq('.', field("member", $.identifier)),
      field("member", $.map_key),
    ),

    _old_value: $ => seq(
      '@',
      field("old_value", $._pattern),
    ),


    /// m[k]
    expr_map_access: $ => prec.left('EXPR_APP', seq(
      field("map", $._expression_simple),
      field("key", $.map_key)
    )),

    map_key: $ => brackets(
      $,
      field("key", $._expression_simple),
      optional($._map_default_value),
    ),

    _map_default_value: $ => seq(
      '=',
      field("default", $._expression)
    ),


    /// r.f
    expr_projection: $ => prec.left('EXPR_APP', seq(
      field("expr", $._expression_simple), '.',
      field("field", $.identifier)
    )),


    /// if(x)
    ///   if(y) 3
    /// else
    ///   2
    expr_if: $ => prec.right(seq(
      'if',
      field_miss(
        $, parens($, field("cond", $._expression)),
        "then", $._expression_body),
      repeat(
        choice(
          $._inhibit_vsemi,
          $.expr_elif
        )
      ),
      optional($.expr_else),
    )),

    _if_branch: $ => choice($.expr_elif, $.expr_else),

    expr_elif: $ => seq(
      'elif',
      field_miss(
        $, parens($, field("cond", $._expression)),
        "then", $._expression_body),
    ),

    expr_else: $ => field_miss($, 'else', "then", $._expression_body),

    /// switch(x) y => y
    expr_switch: $ => seq(
      'switch',
      field("exprs", $.switch_exprs),
      field("cases", $.switch_cases)
    ),

    switch_exprs: $ => parens_comma1($, field("expr", $._expression)),

    switch_cases: $ => maybe_block($, field("case", $.switch_case)),

    switch_case: $ => seq(
      field("patterns", $.switch_patterns),
      field("branch", $._switch_branch),
    ),

    switch_patterns: $ => sep_comma1($, field("pattern", $._pattern)),

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


    /// We allow meaningless letval expressions to relief the parser. It's the AST builder which
    /// will check if that's a statement
    /// let x = 2
    expr_letval: $ => prec.right('EXPR_LETVAL', $._letval),

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
      field("pattern", $._pattern),
      '<-',
      field("expr", $._expression)
    ),

    list_comprehension_if: $ => seq(
      'if',
      parens($, field("cond", $._expression)),
    ),


    /// (1, 2, x)
    expr_tuple: $ => parens_comma($,
      field("elem", $._expression)
    ),


    /// x
    expr_variable: $ => $.qual_identifier,


    /// 1
    expr_literal: $ => alias($._literal, $.expr_literal),


    // We allow tree sitter to parse commonly misplaced INVALID expressions for better error
    // messages
    _INVALID_expr: $ => choice(
      $.INVALID_expr_return,
      $.INVALID_expr_while,
      $.INVALID_expr_for,
    ),

    INVALID_expr_return: $ => prec.right('EXPR_INVALID', seq(
      'return',
       optional($._expression_simple)
    )),

    INVALID_expr_while: $ => prec.right('EXPR_INVALID', seq(
      'while',
      $._INVALID_expr_content,
    )),

    INVALID_expr_for: $ => prec.right('EXPR_INVALID', seq(
      'for',
      $._INVALID_expr_content,
    )),

    _INVALID_expr_content: $ => prec.right('EXPR_INVALID', choice(
      'in', ';', $._expression_simple, parens($, $._INVALID_expr_content),
    )),


    //**************************************************************************
    // PATTERN
    //**************************************************************************

    _pattern: $ => choice(
      $.pat_typed,
      $.pat_op,
      $.pat_application,
      $.pat_match,
      $.pat_map_or_record,
      $.pat_list,
      $.pat_tuple,
      $.pat_variable,
      $.pat_literal,
      $.pat_wildcard,
      alias($._lex_hole, $.pat_hole),
    ),


    /// x + y
    pat_op: $ => choice(
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
            field("op_r", $._pattern),
          ) : seq(
            field("op_l", $._pattern),
            field("op", operator),
            field("op_r", $._pattern),
          )
        )
      )
    ),


    /// x : int
    pat_typed: $ => prec.left(seq(
      field("pattern", $._pattern), ':',
      field("type", $._type)
    )),


    /// f(x, y)
    pat_application: $ => prec.left('PAT_APP', seq(
      field("function", $._pattern),
      field("args", $.pat_args),
    )),

    pat_args: $ => parens_comma(
      $,
      field("arg", $._pattern),
    ),


    pat_map_or_record: $ => braces_comma(
      $,
      field("access", $.member_access)
    ),

    member_access: $ => seq(
      optional(field("path", $.member_path)),
      '=',
      field("pattern", $._pattern)
    ),


    /// x = y
    pat_match: $ => prec.left(seq(
      field("left", $._pattern),
      '=',
      field("right", $._pattern)
    )),


    pat_list: $ => brackets_comma(
      $,
      field("elem", $._pattern)
    ),


    /// (1, 2, x)
    pat_tuple: $ => parens_comma(
      $,
      field("elem", $._pattern),
    ),


    /// x
    pat_variable: $ => $.identifier,


    /// 1
    pat_literal: $ => alias($._literal, $.pat_literal),


    /// _
    pat_wildcard: $ => $._lex_wildcard,


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
      field("domain", $.type_domain),
      '=>',
      field("codomain", $._type)
    )),

    type_domain: $ => choice(
      parens_comma2($, field("arg", $._type)),
      seq('(', $._paren_close),
      field("arg", $._type),
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

    scope_name: $ => alias($._lex_up_id, $.scope_name),

    qual_low_id: $ => seq(
      repeat(seq(field("path", $.scope_name), token.immediate('.'))),
      field("name", alias($._lex_low_id, $.name))
    ),

    qual_up_id: $ => seq(
      repeat(seq(field("path", $.scope_name), token.immediate('.'))),
      field("name", alias($._lex_up_id, $.name))
    ),

    qual_identifier: $ => $.qual_low_id,

    qual_constructor: $ => $.qual_up_id,

    qual_scope_name: $ => $.qual_up_id,


    _paren_close: $ => seq(optional($._inhibit_vclose), ")"),
    _bracket_close: $ => seq(optional($._inhibit_vclose), "]"),
    _brace_close: $ => seq(optional($._inhibit_vclose), "}"),

    vopen: $ => $._vopen,
    vsemi: $ => $._vsemi,
    vclose: $ => $._vclose,

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
