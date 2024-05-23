/* Credits for a huge part of this implementation:
 * https://github.com/alaviss/tree-sitter-nim. Original scanner.c copyright:
 * Leorize <leorize+oss@disroot.org>
 */

#include "scanner.h"

#include <inttypes.h>
#include <stdint.h>
#include <string.h>

#include "tree_sitter/alloc.h"
#include "tree_sitter/parser.h"

#include "indent_vec.h"
#include "preamble.h"
#include "state.h"

#define TRY_LEX_INNER(cnt, ctx, fn, ...)                                       \
  do {                                                                         \
    const uint32_t last_count_##cnt = (ctx)->advance_counter;                  \
    if (fn((ctx), ##__VA_ARGS__)) {                                            \
      return true;                                                             \
    }                                                                          \
    if ((ctx)->advance_counter != last_count_##cnt) {                          \
      return false;                                                            \
    }                                                                          \
  } while (false)
/// Try lexing with the given function.
///
/// If the function succeed, the lexer returns immediately.
/// Otherwise, if no input were consumed, the lexer will continue.
///
/// The lexer will stop immediately if input was consumed and the given lexing
/// function fails.
///
/// @param ctx - The context to monitor state with, and as input to `fn`.
/// @param fn - The lexing function
#define TRY_LEX(ctx, fn, ...) TRY_LEX_INNER(__COUNTER__, ctx, fn, ##__VA_ARGS__)
#define LEX_FN(name, ...)                                                      \
  _nonnull_(1) static bool name(struct context *ctx, ##__VA_ARGS__)

_const_ static bool is_digit(uint32_t chr) { return chr >= '0' && chr <= '9'; }

_const_ static bool is_lower(uint32_t chr) { return chr >= 'a' && chr <= 'z'; }

_const_ static bool is_upper(uint32_t chr) { return chr >= 'A' && chr <= 'Z'; }

_const_ static bool is_keyword(uint32_t chr) {
  return is_lower(chr) || is_upper(chr) || chr == '_';
}

_const_ static bool is_identifier(uint32_t chr) {
  return is_keyword(chr) || is_digit(chr);
}

_const_ static uint32_t to_upper(uint32_t chr) {
  const uint32_t lower_case_bit = 1U << 5U;
  return is_lower(chr) ? chr & ~lower_case_bit : chr;
}

_nonnull_(1) static size_t scan_spaces(struct context *ctx, bool force_update) {
  bool update_indent = force_update;
  uint8_t indent = 0;
  size_t spaces = 0;
  while (true) {
    switch (context_lookahead(ctx)) {
    case ' ':
      indent += (int)(indent != INVALID_INDENT_VALUE);
      spaces++;
      context_advance(ctx, true);
      break;
    case '\n':
    case '\r':
      update_indent = true;
      indent = 0;
      spaces++;
      context_advance(ctx, true);
      break;
    case '\0':
      if (context_eof(ctx)) {
        update_indent = true;
        indent = 0;
      }
      goto loop_end;
    default:
      goto loop_end;
    }
  }
loop_end:
  if (update_indent) {
    DBG_F("updated current indentation: %" PRIu8 "\n", indent);
    ctx->_current_indent = indent;
    ctx->flags |= FLAG_AFTER_NEWLINE;
  }

  return spaces;
}

LEX_FN(lex_long_string_quote) {
  if (context_lookahead(ctx) != '"' ||
      !valid_tokens_test(ctx->valid_tokens, LONG_STRING_QUOTE)) {
    return false;
  }

  context_consume(ctx, false);
  uint8_t count = 1;
  while (context_lookahead(ctx) == '"' && count < 3) {
    context_advance(ctx, false);
    count++;
  }

  if (count < 3) {
    context_mark_end(ctx);
    return context_finish(ctx, LONG_STRING_QUOTE);
  }

  if (context_lookahead(ctx) == '"') {
    return context_finish(ctx, LONG_STRING_QUOTE);
  }

  return false;
}

static const struct valid_tokens COMMENT_TOKENS = VALID_TOKENS(
    TO_VT_BIT(BLOCK_COMMENT_CONTENT) | TO_VT_BIT(BLOCK_DOC_COMMENT_CONTENT) |
    TO_VT_BIT(COMMENT_CONTENT));

LEX_FN(lex_comment_content) {
  if (!valid_tokens_any_valid(ctx->valid_tokens, COMMENT_TOKENS) ||
      valid_tokens_is_error(ctx->valid_tokens)) {
    return false;
  }

  if (valid_tokens_test(ctx->valid_tokens, COMMENT_CONTENT)) {
    while (!context_eof(ctx)) {
      switch (context_lookahead(ctx)) {
      case '\n':
      case '\r':
        goto exit_short_comment_loop;
      default:
        context_advance(ctx, false);
      }
    }

  exit_short_comment_loop:
    context_mark_end(ctx);
    return context_finish(ctx, COMMENT_CONTENT);
  }

  uint32_t nesting = 0;
  while (!context_eof(ctx)) {
    if (context_lookahead(ctx) == '/' && context_advance(ctx, false) == '*') {
      nesting++;
      DBG_F("block comment nest level: %" PRIu32 "\n", nesting);
    }
    context_mark_end(ctx);
    if (context_lookahead(ctx) == '*') {
      if (context_advance(ctx, false) == '/') {
        if (nesting > 0) {
          DBG_F("block comment terminate nest level: %" PRIu32 "\n", nesting);
          nesting--;
        } else if (valid_tokens_test(ctx->valid_tokens,
                                     BLOCK_DOC_COMMENT_CONTENT)) {
          return context_finish(ctx, BLOCK_DOC_COMMENT_CONTENT);
        } else {
          return context_finish(ctx, BLOCK_COMMENT_CONTENT);
        }
      }
      continue;
    }
    context_advance(ctx, false);
  }

  return false;
}

LEX_FN(lex_init) {
  if (ctx->state->layout_stack.len > 0 ||
      valid_tokens_is_error(ctx->valid_tokens) ||
      valid_tokens_any_valid(ctx->valid_tokens, COMMENT_TOKENS)) {
    return false;
  }

  scan_spaces(ctx, true);

  if (context_lookahead(ctx) == '/') {
    return false;
  }

  indent_value current_indent = context_indent(ctx);
  if (current_indent == INVALID_INDENT_VALUE) {
    DBG("no valid indentation found");
    return false;
  }

  if (indent_vec_push(&ctx->state->layout_stack, current_indent) < 0) {
    DBG("could not extend layout stack");
    return false;
  }

  return context_finish(ctx, SYNCHRONIZE);
}

static bool chrcaseeq(uint32_t lhs, uint32_t rhs) {
  return to_upper(lhs) == to_upper(rhs);
}

LEX_FN(scan_continuing_keyword) {
#define NEXT_OR_FAIL(chr)                                                      \
  do {                                                                         \
    context_advance(ctx, false);                                               \
    if (!chrcaseeq(context_lookahead(ctx), chr)) {                             \
      return false;                                                            \
    }                                                                          \
  } while (false)

#define FINISH_IF_END                                                          \
  do {                                                                         \
    context_advance(ctx, false);                                               \
    return !is_identifier(context_lookahead(ctx));                             \
  } while (false)

  if (context_lookahead(ctx) == 'e') {
    context_advance(ctx, false);
    if (chrcaseeq(context_lookahead(ctx), 'l')) {
      context_advance(ctx, false);
      if (chrcaseeq(context_lookahead(ctx), 's')) {
        NEXT_OR_FAIL('e');
        FINISH_IF_END;
      } else if (chrcaseeq(context_lookahead(ctx), 'i')) {
        NEXT_OR_FAIL('f');
        FINISH_IF_END;
      }

      return false;
    }
  }

  return false;

#undef CASE_CHAR
#undef NEXT_OR_FAIL
#undef FINISH_IF_END
}

const static struct valid_tokens NO_VCLOSE_CTX =
    VALID_TOKENS(TO_VT_BIT(INHIBIT_VCLOSE) | TO_VT_BIT(LONG_STRING_QUOTE));

LEX_FN(lex_indent_query) {
  if (valid_tokens_is_error(ctx->valid_tokens)) {
    return false;
  }

  if (ctx->state->layout_stack.len == 0) {
    return false;
  }

  if (context_lookahead(ctx) == '/') {
    return false;
  }

  indent_value current_layout = indent_vec_back(&ctx->state->layout_stack);

  indent_value current_indent = context_indent(ctx);

  if (valid_tokens_test(ctx->valid_tokens, LAYOUT_NOT_AT_LEVEL) &&
      current_indent > current_layout && !(ctx->flags & FLAG_AFTER_NEWLINE)) {
    return context_finish(ctx, LAYOUT_NOT_AT_LEVEL);
  }

  if (valid_tokens_test(ctx->valid_tokens, LAYOUT_AT_LEVEL) &&
      current_indent == current_layout && (ctx->flags & FLAG_AFTER_NEWLINE)) {
    return context_finish(ctx, LAYOUT_AT_LEVEL);
  }

  return false;
}

// This function is big by design.
//
// NOLINTNEXTLINE(*-cognitive-complexity)
LEX_FN(lex_indent) {
  if (ctx->state->layout_stack.len == 0) {
    return false;
  }

  // TODO: this accidentally allows the div operator to be misindented.
  // If should be checked if the characer after is indeed a comment start
  if (context_lookahead(ctx) == '/') {
    return false;
  }

  indent_value current_layout = indent_vec_back(&ctx->state->layout_stack);

  indent_value current_indent = context_indent(ctx);

  if (current_indent == INVALID_INDENT_VALUE) {
    return false;
  }

  if (valid_tokens_test(ctx->valid_tokens, VOPEN)) {
    if (current_indent > current_layout) {
      if (indent_vec_push(&ctx->state->layout_stack, current_indent) < 0) {
        DBG("could not extend layout stack");
        return false;
      }
      return context_finish(ctx, VOPEN);
    }
  }

  if (valid_tokens_test(ctx->valid_tokens, LAYOUT_EMPTY)) {
    switch (0) {
    default:
      if (valid_tokens_is_error(ctx->valid_tokens)) {
        break;
      }
      if (current_indent <= current_layout) {
        return context_finish(ctx, LAYOUT_EMPTY);
      }
    }
  }

  if (valid_tokens_test(ctx->valid_tokens, VSEMI)) {
    if (current_indent <= current_layout) {
      if (current_indent == current_layout) {
        if (valid_tokens_test(ctx->valid_tokens, INHIBIT_VSEMI) &&
            scan_continuing_keyword(ctx)) {
          DBG("found continuing keyword");
          return false;
        }
      }
      return context_finish(ctx, VSEMI);
    }
  }

  // Implicit layout changes
  if (!valid_tokens_any_valid(ctx->valid_tokens, NO_VCLOSE_CTX) ||
      valid_tokens_is_error(ctx->valid_tokens) ||
      // Allow EOF to force a layout_end, which would lead to better error
      // recovery
      context_eof(ctx)) {
    // VCLOSE
    if (current_indent < current_layout || context_eof(ctx)) {
      if (ctx->state->layout_stack.len > 1) {
        indent_vec_pop(&ctx->state->layout_stack);
        return context_finish(ctx, VCLOSE);
      }
    }
  }

  return false;
}

LEX_FN(lex_inline_layout) {
  if (ctx->state->layout_stack.len == 0 || (ctx->flags & FLAG_AFTER_NEWLINE)) {
    return false;
  }

  switch (context_lookahead(ctx)) {
  case ',':
    if (valid_tokens_test(ctx->valid_tokens, COMMA)) {
      return false;
    }
    break;
  case ')':
  case ']':
  case '}':
    break;
  case '.':
    if (context_advance(ctx, false) == '}') {
      break;
    }
    return false;
  default:
    if (!valid_tokens_test(ctx->valid_tokens, INHIBIT_VSEMI) &&
        scan_continuing_keyword(ctx)) {
      break;
    }
    return false;
  }
  if (valid_tokens_test(ctx->valid_tokens, VSEMI)) {
    DBG("terminate via inline element");
    return context_finish(ctx, VSEMI);
  }

  if (valid_tokens_test(ctx->valid_tokens, VCLOSE) &&
      ctx->state->layout_stack.len > 1) {
    DBG("end layout via inline element");
    indent_vec_pop(&ctx->state->layout_stack);
    return context_finish(ctx, VCLOSE);
  }

  return false;
}

LEX_FN(lex_main) {
  TRY_LEX(ctx, lex_init);

  TRY_LEX(ctx, lex_comment_content);
  TRY_LEX(ctx, lex_long_string_quote);

  scan_spaces(ctx, false);

  TRY_LEX(ctx, lex_indent_query);
  TRY_LEX(ctx, lex_indent);
  TRY_LEX(ctx, lex_inline_layout);

  return false;
}

void *tree_sitter_aesophia_external_scanner_create(void) {
#ifdef TREE_SITTER_INTERNAL_BUILD
  debug_mode = getenv("TREE_SITTER_DEBUG");
#endif

  struct state *state = state_new();
  if (!state) {
    DBG("error: could not allocate a new state object!");
  }
  return state;
}

void tree_sitter_aesophia_external_scanner_destroy(void *payload) {
  state_destroy((struct state *)payload);
}

unsigned tree_sitter_aesophia_external_scanner_serialize(void *payload,
                                                         uint8_t *buffer) {
  if (!payload || !buffer) {
    DBG("error: no payload or buffer");
    return 0;
  }
  return state_serialize((struct state *)payload, buffer,
                         TREE_SITTER_SERIALIZATION_BUFFER_SIZE);
}

void tree_sitter_aesophia_external_scanner_deserialize(void *payload,
                                                       const uint8_t *buffer,
                                                       unsigned length) {
  if (!payload) {
    DBG("no payload, skipping");
    return;
  }
  state_deserialize((struct state *)payload, buffer, length);
}

bool tree_sitter_aesophia_external_scanner_scan(void *payload, TSLexer *lexer,
                                                const bool *valid_tokens) {
  if (!payload || !lexer || !valid_tokens) {
    DBG("error: some parameters are not provided");
    return false;
  }

  DBG("begin");
  struct context ctx = {0};
  ctx._lexer = lexer;
  ctx.state = (struct state *)payload;
  ctx.valid_tokens = valid_tokens_from_array(valid_tokens);

  valid_tokens_debug(ctx.valid_tokens);
  state_debug(ctx.state);
  context_mark_end(&ctx);

  bool found = lex_main(&ctx);

  DBG(found ? "commit" : "end");
  state_debug(ctx.state);
  return found;
}
