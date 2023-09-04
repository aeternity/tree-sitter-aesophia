const block = ($, rule) => seq(
  $.block_open,
  sep1(rule, $.block_semi),
  $.block_close,
);

const inline_block = ($, rule) => seq(
  $.block_open_inline,
  sep1(rule, $.block_semi),
  $.block_close,
);

const weak_block = ($, rule) => choice(
  inline_block($, rule),
  block($, rule),
);

const block_or = ($, elem, single) => choice(
  block($, elem),
  single
);

const maybe_block = ($, rule) => block_or($, rule, rule);

const sep2 = (rule, delimiter) => seq(
  rule,
  repeat1(seq(delimiter, rule))
);

const sep1 = (rule, delimiter) => seq(
  repeat(seq(rule, delimiter)),
  rule,
);

const sep = (rule, delimiter) => optional(sep1(rule, delimiter));

const lex_dispath_begin = '@ts.parse(';
const lex_dispath_end = ')';

const dispath = (trigger, rule) => seq(
  lex_dispath_begin,
  trigger,
  lex_dispath_end,
  field(trigger, rule),
);

const parens = (...rule) => seq(
  '(',
  ...rule,
  ')'
);

const braces = (...rule) => seq(
  '{',
  ...rule,
  '}'
);

const brackets = (...rule) => seq(
  '[',
  ...rule,
  ']'
);

const wrap_comma = (open, rule, close) => seq(
  open,
  repeat(seq(rule, ',')),
  optional(rule),
  close,
);

const wrap_comma1 = (open, rule, close) => seq(
  open,
  rule,
  repeat(seq(',', rule)),
  optional(','),
  close,
);

const wrap_comma2 = (open, rule, close) => seq(
  open,
  rule,
  repeat1(seq(',', rule)),
  optional(','),
  close,
);

const parens_comma = (rule) => seq(
  '(',
  repeat(seq(rule, ',')),
  optional(rule),
  ')'
);

const parens_comma1 = (rule) => seq(
  '(',
  rule,
  repeat(seq(',', rule)),
  optional(','),
  ')'
);

const parens_comma2 = (rule) => seq(
  '(',
  rule,
  repeat1(seq(',', rule)),
  optional(','),
  ')'
);

const braces_comma = (rule) => seq(
  '{',
  repeat(seq(rule, ',')),
  optional(rule),
  '}'
);

const braces_comma1 = (rule) => seq(
  '{',
  rule,
  repeat(seq(',', rule)),
  optional(','),
  '}'
);

const braces_comma2 = (rule) => seq(
  '{',
  rule,
  repeat1(seq(',', rule)),
  optional(','),
  '}'
);

const brackets_comma = (rule) => seq(
  '[',
  repeat(seq(rule, ',')),
  optional(rule),
  ']'
);

const brackets_comma1 = (rule) => seq(
  '[',
  rule,
  repeat(seq(',', rule)),
  optional(','),
  ']'
);

const brackets_comma2 = (rule) => seq(
  '[',
  rule,
  repeat1(seq(',', rule)),
  optional(','),
  ']'
);

const qual = (q, n) => seq(
  repeat(seq(q, token.immediate('.'))),
  n
);

const qual1 = (q, n) => seq(
  repeat1(seq(q, token.immediate('.'))),
  n
);

module.exports = {
  parens,
  braces,
  brackets,
  wrap_comma,
  wrap_comma1,
  wrap_comma2,
  parens_comma,
  parens_comma1,
  parens_comma2,
  braces_comma,
  braces_comma1,
  braces_comma2,
  brackets_comma,
  brackets_comma1,
  brackets_comma2,
  block,
  block_or,
  maybe_block,
  weak_block,
  inline_block,
  dispath,
  sep,
  sep1,
  sep2,
  qual,
  qual1,
};
