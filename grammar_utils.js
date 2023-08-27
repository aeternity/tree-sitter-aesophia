// const block = ($, rule) => seq(
//   $._newline,
//   $._block_open,
//   sep1(rule, seq($._newline, $._block_semi)),
//   $._newline,
//   $._block_close
// );

const block = ($, rule) => seq(
  alias($._block_open, $.OPEN),
  sep1(rule, alias($._block_semi, $.SEMI)),
  alias($._block_close, $.CLOSE)
);

const inline_block = ($, rule) => seq(
  alias($._block_open_inline, $.OPEN),
  sep1(rule, alias($._block_semi, $.SEMI)),
  alias($._block_close, $.CLOSE)
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
  rule,
  repeat(seq(delimiter, rule))
);

const sep = (rule, delimiter) => optional(sep1(rule, delimiter));

const lex_dispath_begin = '@ts.parse(';
const lex_dispath_end = ')';

function dispath(trigger, rule) {
  return seq(
    lex_dispath_begin,
    trigger,
    lex_dispath_end,
    field("parsed", rule),
  );
}

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
