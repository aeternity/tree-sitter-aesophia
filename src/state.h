#ifndef __AESOPHIA_TS_STATE
#define __AESOPHIA_TS_STATE

#include "tree_sitter/alloc.h"
#include "tree_sitter/parser.h"

#include "indent_vec.h"
#include "preamble.h"

#define FLAG_LEN 1U
#define FLAG_AFTER_NEWLINE 1U

typedef uint8_t flags_storage;

struct valid_tokens {
  uint32_t bits : TOKEN_TYPE_LEN;
};

#define TO_VT_BIT(value) 1U << (enum token_type)(value)
#define VALID_TOKENS(bits_)                                                    \
  { .bits = (bits_) }

_nonnull_(1) _pure_ static struct valid_tokens
    valid_tokens_from_array(const bool *valid_tokens) {
  struct valid_tokens result = {0};
  for (unsigned i = TOKEN_TYPE_START; i < TOKEN_TYPE_LEN; i++) {
    result.bits |= (unsigned)valid_tokens[i] << i;
  }
  return result;
}

_const_ static bool valid_tokens_test(struct valid_tokens self,
                                      enum token_type type) {
  return (self.bits & TO_VT_BIT(type)) != 0;
}

_const_ static bool valid_tokens_any_valid(struct valid_tokens left,
                                           struct valid_tokens right) {
  return (left.bits & right.bits) != 0;
}

_const_ static bool valid_tokens_is_error(struct valid_tokens self) {
  return self.bits == ~(~0U << (enum token_type)TOKEN_TYPE_LEN);
}

static void valid_tokens_debug(struct valid_tokens self) {
  if (debug_mode) {
    DBG("valid tokens: [");
    for (size_t i = TOKEN_TYPE_START; i < TOKEN_TYPE_LEN; i++) {
      if (valid_tokens_test(self, i)) {
        (void)dprintf(" %s", TOKEN_TYPE_STR[i]);
      }
    }
    (void)dputs(" ]\n");
  }
}

struct state {
  struct indent_vec layout_stack;
};

static struct state *state_new(void) {
  struct state *result = ts_calloc(1, sizeof(struct state));
  if (!result) {
    return NULL;
  }
  return result;
}

static void state_destroy(struct state *self) {
  if (self) {
    indent_vec_destroy(&self->layout_stack);
    ts_free(self);
  }
}

_nonnull_(1) static void state_clear(struct state *self) {
  indent_vec_set_len(&self->layout_stack, 0);
}

_nonnull_(1, 2) static unsigned state_serialize(const struct state *self,
                                                uint8_t *buffer,
                                                unsigned buffer_len) {
  unsigned serialize_len = 0;
  serialize_len += indent_vec_serialize(
      &self->layout_stack, &buffer[serialize_len], buffer_len - serialize_len);
  DBG_F("serialized %u bytes\n", serialize_len);
  return serialize_len;
}

_nonnull_(1) static void state_deserialize(struct state *self,
                                           const uint8_t *buffer,
                                           unsigned buffer_len) {
  if (!buffer && buffer_len > 0) {
    DBG("error: no buffer but buffer length > 0");
    return;
  }

  unsigned idx = 0;

  state_clear(self);
  indent_vec_deserialize(&self->layout_stack, &buffer[idx], buffer_len - idx);
}

_nonnull_(1) static void state_debug(struct state *self) {
  indent_vec_debug(&self->layout_stack);
}

struct context {
  TSLexer *_lexer;
  struct state *state;
  uint32_t advance_counter;
  struct valid_tokens valid_tokens;
  indent_value _current_indent;
  flags_storage flags : FLAG_LEN;
};

_nonnull_(1) static void context_mark_end(struct context *self) {
  self->_lexer->mark_end(self->_lexer);
}

_nonnull_(1) _pure_ static uint32_t context_lookahead(struct context *self) {
  return (uint32_t)self->_lexer->lookahead;
}

_nonnull_(1) _pure_ static bool context_eof(struct context *self) {
  return self->_lexer->eof(self->_lexer);
}

_nonnull_(1) static uint32_t context_advance(struct context *self, bool skip) {
  self->advance_counter += !context_eof(self);
  if (!context_eof(self)) {
    self->flags &= ~FLAG_AFTER_NEWLINE;
  }
  self->_lexer->advance(self->_lexer, skip);
  return (uint32_t)self->_lexer->lookahead;
}

_nonnull_(1) static uint32_t context_consume(struct context *self, bool skip) {
  uint32_t result = context_advance(self, skip);
  context_mark_end(self);
  return result;
}

_nonnull_(1) static bool context_finish(struct context *self,
                                        enum token_type type) {
  DBG_F("finished scanning token: %s\n", TOKEN_TYPE_STR[type]);
  self->_lexer->result_symbol = (TSSymbol)type;
  return true;
}

_nonnull_(1) static indent_value context_indent(struct context *self) {
  if (self->flags & FLAG_AFTER_NEWLINE) {
    return self->_current_indent;
  }

  return INVALID_INDENT_VALUE;
}

#endif // __AESOPHIA_TS_STATE
