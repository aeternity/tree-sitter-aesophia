#ifndef __AESOPHIA_TS_PREAMBLE
#define __AESOPHIA_TS_PREAMBLE

#include <stdbool.h>

#ifdef __GNUC__

#pragma GCC diagnostic ignored "-Wgnu-zero-variadic-macro-arguments"

#define _nonnull_(...) __attribute__((nonnull(__VA_ARGS__)))
#define _returns_nonnull_ __attribute__((returns_nonnull))
#define _const_ __attribute__((const))
#define _pure_ __attribute__((pure))

#else

#define _nonnull_(...)
#define _returns_nonnull_
#define _const_
#define _pure_

#endif

#ifdef TREE_SITTER_INTERNAL_BUILD

#define dprintf(...) fprintf(stderr, __VA_ARGS__)
#define dputs(msg) fputs(msg, stderr)
#define DBG(msg)                                                               \
  if (debug_mode)                                                              \
  (void)fprintf(stderr, "lex_aesophia: %s():%d: %s\n", __func__, __LINE__, msg)
#define DBG_F(fmt, ...)                                                        \
  if (debug_mode)                                                              \
  (void)fprintf(stderr, "lex_aesophia: %s():%d: " fmt, __func__, __LINE__,     \
                ##__VA_ARGS__)

#define RUNTIME_ASSERT(cond)                                                   \
  if (!(cond)) {                                                               \
    (void)fprintf(stderr, "lex_aesophia: %s():%d: Assertion `%s' failed.\n",   \
                  __func__, __LINE__, #cond);                                  \
    abort();                                                                   \
  }

static bool debug_mode = false; /* NOLINT(*-global-variables) */

#else

#define dprintf(...) ((void)0)
#define dputs(msg) ((void)0)
#define DBG(msg) ((void)0)
#define DBG_F(fmt, ...) ((void)0)
static const bool debug_mode = false;
#define RUNTIME_ASSERT(cond) ((void)(0))

#endif

#define MIN(left, right) ((left) > (right) ? (right) : (left))
#define MAX(left, right) ((left) < (right) ? (right) : (left))


enum token_type {
  TOKEN_TYPE_START, // hack to get the size of this enum
  BLOCK_COMMENT_CONTENT = TOKEN_TYPE_START,
  BLOCK_DOC_COMMENT_CONTENT,
  COMMENT_CONTENT,
  LONG_STRING_QUOTE,
  VOPEN,
  VCLOSE,
  VSEMI,
  LAYOUT_AT_LEVEL,
  LAYOUT_NOT_AT_LEVEL,
  LAYOUT_EMPTY,
  INHIBIT_VCLOSE,
  INHIBIT_VSEMI,
  COMMA,
  SYNCHRONIZE,
  INVALID_LAYOUT,
  UNARY_OP,
  TOKEN_TYPE_LEN // hack to get the size of this enum
};


#ifdef TREE_SITTER_INTERNAL_BUILD
const char *const TOKEN_TYPE_STR[TOKEN_TYPE_LEN] = {
    "BLOCK_COMMENT_CONTENT",
    "BLOCK_DOC_COMMENT_CONTENT",
    "COMMENT_CONTENT",
    "LONG_STRING_QUOTE",
    "VOPEN",
    "VCLOSE",
    "VSEMI",
    "LAYOUT_AT_LEVEL",
    "LAYOUT_NOT_AT_LEVEL",
    "LAYOUT_EMPTY",
    "INHIBIT_VCLOSE",
    "INHIBIT_VSEMI",
    "COMMA",
    "SYNCHRONIZE",
    "INVALID_LAYOUT",
    "UNARY_OP",
};
#endif // TREE_SITTER_INTERNAL_BUILD


#endif // __AESOPHIA_TS_PREAMBLE
