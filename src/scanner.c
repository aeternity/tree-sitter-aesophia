#include "tree_sitter/parser.h"
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#define DEBUG

#define ever (;;)

#define MAX(a, b) ((a) > (b) ? (a) : (b))

#define VEC_RESIZE(vec, _cap)                                                  \
    void *tmp = realloc((vec).data, (_cap) * sizeof((vec).data[0]));           \
    assert(tmp != NULL);                                                       \
    (vec).data = tmp;                                                          \
    assert((vec).data != NULL);                                                \
    (vec).cap = (_cap);

#define VEC_GROW(vec, _cap)                                                    \
    if ((vec).cap < (_cap)) {                                                  \
        VEC_RESIZE((vec), (_cap));                                             \
    }

#define VEC_PUSH(vec, el)                                                      \
    if ((vec).cap == (vec).len) {                                              \
        VEC_RESIZE((vec), MAX(16, (vec).len * 2));                             \
    }                                                                          \
    (vec).data[(vec).len++] = (el);

#define VEC_POP(vec) (vec).len--;

#define VEC_BACK(vec) ((vec).data[(vec).len - 1])

#define VEC_FREE(vec)                                                          \
    {                                                                          \
        if ((vec).data != NULL)                                                \
            free((vec).data);                                                  \
    }

#define VEC_CLEAR(vec) (vec).len = 0;


enum TokenType {
  BLOCK_OPEN,
  BLOCK_INDENT,
  BLOCK_SEMI,
  BLOCK_CLOSE,
  BLOCK_COMMENT_CONTENT,
  ERROR_STATE
};

typedef struct {
  uint32_t len;
  uint32_t cap;
  uint16_t * data;
}
vec;

typedef struct {
  vec indents;
  size_t pending_dedents;
}
Scanner;


#ifdef DEBUG
static void print_scanner(Scanner* scanner) {
  printf("INDENTS:");
  for(size_t i = 0; i < scanner->indents.len; ++i) {
    printf(" %d", scanner->indents.data[i]);
  }
  printf("\nDEDENTS: %ld\n", scanner->pending_dedents);
}

static void print_symbols(const bool *valid_symbols) {
  printf("VALID SYMBOLS:\n");
  printf("    BLOCK_OPEN: %d\n", valid_symbols[BLOCK_OPEN]);
  printf("    BLOCK_INDENT: %d\n", valid_symbols[BLOCK_INDENT]);
  printf("    BLOCK_SEMI: %d\n", valid_symbols[BLOCK_SEMI]);
  printf("    BLOCK_CLOSE: %d\n", valid_symbols[BLOCK_CLOSE]);
  printf("    BLOCK_COMMENT_CONTENT: %d\n", valid_symbols[BLOCK_COMMENT_CONTENT]);
  printf("    ERROR_STAT: %d\n", valid_symbols[ERROR_STATE]);
}

static void print_symbol(int t) {
  switch(t) {
  case BLOCK_OPEN:
    printf("BLOCK_OPEN");
    break;
  case BLOCK_INDENT:
    printf("BLOCK_INDENT");
    break;
  case BLOCK_SEMI:
    printf("BLOCK_SEMI");
    break;
  case BLOCK_CLOSE:
    printf("BLOCK_CLOSE");
    break;
  case BLOCK_COMMENT_CONTENT:
    printf("BLOCK_COMMENT_CONTENT");
    break;
  case ERROR_STATE:
    printf("ERROR_STAT");
    break;
  }
}
#else
#define printf(...)
#define print_symbol(...)
#define print_symbols(...)
#define print_scanner(...)
#endif

static inline void advance(TSLexer * lexer) {
  lexer->advance(lexer, false);
}

static inline void skip(TSLexer * lexer) {
  lexer->advance(lexer, true);
}

// Parse a nested block comment. Coursor is looking at the first '/'.  This is called only when
// already in a block comment.
static bool scan_block_comment(TSLexer * lexer) {
  // Accept progress so far
  lexer->mark_end(lexer);

  // Check if we are indeed opening a new comment block
  if (lexer->lookahead != '/') {
    goto NOT_ACCEPT;
  }
  advance(lexer);
  if (lexer->lookahead != '*') {
    goto NOT_ACCEPT;
  }
  advance(lexer);

  // Eat the comment
  while(true) {

    // Unclosed block comment
    if (lexer->eof(lexer)) {
      goto ACCEPT;
    }

    switch (lexer->lookahead) {
    case '/':
      // A possibility of opening a more nested block comment. Try to include it.
      scan_block_comment(lexer);
      break;
    case '*':
      // A possibility of closing the block comment.
      advance(lexer);
      if (lexer->lookahead == '/') {
        advance(lexer);
        goto ACCEPT;
      }
      break;
    default:
      advance(lexer);
    }
  }

  NOT_ACCEPT:
    return false;
  ACCEPT:
    return true;
}

static void advance_to_line_end(TSLexer * lexer) {
  while(true) {
    if (lexer->lookahead == '\n' || lexer->eof(lexer)) {
      break;
    }
    advance(lexer);
  }
}

static bool scan(Scanner * scanner,
                 TSLexer * lexer,
                 const bool * valid_symbols) {

    printf("INIT SCAN, column %d\n", lexer->get_column(lexer));
    print_scanner(scanner);
    print_symbols(valid_symbols);
    printf("\n");

    if (valid_symbols[ERROR_STATE]) {
      goto NOT_ACCEPT;
    }

    // First handle eventual runback pending_dedents
    if (scanner->pending_dedents > 0 &&
      valid_symbols[BLOCK_CLOSE]) {
      printf("FOUND PENDING DEDENT\n");
      scanner->pending_dedents--;
      lexer->result_symbol = BLOCK_CLOSE;
      goto ACCEPT;
    }

    if (lexer->eof(lexer) &&
      valid_symbols[BLOCK_CLOSE]) {
      printf("EOF DEDENT\n");
      scanner->pending_dedents--;
      lexer->result_symbol = BLOCK_CLOSE;
      goto ACCEPT;
    }

    // Check if we have newlines and how much indentation
    bool has_line_end = false;
    bool can_call_mark_end = true;
    uint32_t indent = 0;
    lexer->mark_end(lexer);

    // Skip to the first line with something meaningful
    while(true) {
      if (lexer->lookahead == ' ' || lexer->lookahead == '\r') {
        // Skip all whitespaces
        skip(lexer);
      } else if (lexer->lookahead == '\t') {
        // No tabs
        goto NOT_ACCEPT;
      } else if (lexer->lookahead == '\n') {
        if(valid_symbols[BLOCK_OPEN]) {
          lexer->mark_end(lexer);
          lexer->result_symbol = BLOCK_OPEN;
          goto ACCEPT;
        }
        // Calculate indent
        skip(lexer);
        has_line_end = true;
        while(true) {
          if (lexer->lookahead == ' ') {
            skip(lexer);
          } else {
            indent = lexer->get_column(lexer);
            break;
          }
        }
      } else if (!valid_symbols[BLOCK_COMMENT_CONTENT] &&
        lexer->lookahead == '/') {
        // Scan past line comments. As far as the special token
        // types we're scanning for here are concerned line comments
        // are like whitespace. There is nothing useful to be
        // learned from, say, their indentation. So we advance past
        // them here.
        //
        // The one thing we need to keep in mind is that we should
        // not call `lexer->mark_end(lexer)` after this point, or
        // the comment will be lost.
        advance(lexer);

        if (lexer->lookahead == '/' && has_line_end) {
          // The first thing in this line is a line comment. This should not affect indentation.
          can_call_mark_end = false;
          advance(lexer);
          advance_to_line_end(lexer);
        } else {
          goto NOT_ACCEPT;
        }
      } else if (valid_symbols[BLOCK_COMMENT_CONTENT] &&
                 lexer->lookahead == '*') {
        // Block comment end candidate
        advance(lexer);
        if(lexer->lookahead == '/') {
          lexer->result_symbol = BLOCK_COMMENT_CONTENT;
          goto ACCEPT;
        } else {
          goto NOT_ACCEPT;
        }
      } else if (lexer->eof(lexer)) {
        has_line_end = true;
        indent = 0;

        if (valid_symbols[BLOCK_COMMENT_CONTENT]) {
          lexer->result_symbol = BLOCK_COMMENT_CONTENT;
          goto ACCEPT;
        }
        break;
      } else {
        break;
      }
    }

    if (valid_symbols[BLOCK_COMMENT_CONTENT]) {
        if (!can_call_mark_end) {
            goto NOT_ACCEPT;
        }
        lexer->mark_end(lexer);
        while(true) {
            if (lexer->lookahead == '\0') {
                break;
            }
            if (lexer->lookahead != '/' && lexer->lookahead != '*') {
                advance(lexer);
            } else if (lexer->lookahead == '*') {
                lexer->mark_end(lexer);
                advance(lexer);
                if (lexer->lookahead == '/') {
                    break;
                }
            } else if (scan_block_comment(lexer)) {
                lexer->mark_end(lexer);
                advance(lexer);
                if (lexer->lookahead == '*') {
                    break;
                }
            }
        }

        lexer->result_symbol = BLOCK_COMMENT_CONTENT;
        goto ACCEPT;
    }

    if (has_line_end) {
      printf("NEWLINE; indent %d\n", indent);
      if (indent > VEC_BACK(scanner->indents) &&
        valid_symbols[BLOCK_INDENT] && !lexer->eof(lexer)) {
        VEC_PUSH(scanner->indents, lexer->get_column(lexer));
        lexer->result_symbol = BLOCK_INDENT;
        goto ACCEPT;
      } else if (indent == VEC_BACK(scanner->indents) &&
        valid_symbols[BLOCK_SEMI] && !lexer->eof(lexer)) {
        /* // Don't insert BLOCK_SEMI when there is a line */
        /* // comment incoming */

        /* if (lexer->lookahead == '/') { */
        /*   skip(lexer); */
        /*   if (lexer->lookahead == '/') { */
        /*     goto NOT_ACCEPT; */
        /*   } */
        /* } */
        /* // Don't insert BLOCK_SEMI when there is a block */
        /* // comment incoming */
        /* if (lexer->lookahead == '/') { */
        /*   skip(lexer); */
        /*   if (lexer->lookahead == '*') { */
        /*     goto NOT_ACCEPT; */
        /*   } */
        /* } */

        lexer->result_symbol = BLOCK_SEMI;
        goto ACCEPT;
      } else if (indent < VEC_BACK(scanner->indents)) {
        printf("LOWER INDENT: %d < %d\n", indent, VEC_BACK(scanner->indents));
        while (indent < VEC_BACK(scanner->indents)) {
          scanner->pending_dedents++;
          VEC_POP(scanner->indents);

          printf("DECREASED TO %d\n", VEC_BACK(scanner->indents));
        }
        if (indent == VEC_BACK(scanner->indents)) {
          printf("FOUND MATCHING\n");
          lexer->result_symbol = BLOCK_CLOSE;
          scanner->pending_dedents--;
          goto ACCEPT;
        } else {
          goto NOT_ACCEPT;
        }
      }
    }

 NOT_ACCEPT:
    printf("NOT\n\n");
    return false;
 ACCEPT:
        printf("ACCEPT: ");
        print_symbol(lexer->result_symbol);
        printf("\n\n");
        return true;
}

    // --------------------------------------------------------------------------------------------------------
    // API
    // --------------------------------------------------------------------------------------------------------

    /**
     * This function allocates the persistent state of the parser that is passed
     * into the other API functions.
     */
void * tree_sitter_aesophia_external_scanner_create() {
  Scanner * scanner = (Scanner * ) calloc(1, sizeof(Scanner));
  return scanner;
}

    /**
     * Main logic entry point.
     * Since the state is a singular vector, it can just be cast and used directly.
     */
bool tree_sitter_aesophia_external_scanner_scan(void * payload, TSLexer * lexer,
                                                const bool * valid_symbols) {
  Scanner * scanner = (Scanner * ) payload;
  return scan(scanner, lexer, valid_symbols);
}

    /**
     * Copy the current state to another location for later reuse.
     * This is normally more complex, but since this parser's state constists solely
     * of a vector of integers, it can just be copied.
     */
unsigned tree_sitter_aesophia_external_scanner_serialize(void * payload,
                                                         char * buffer) {
  Scanner * scanner = (Scanner * ) payload;
  size_t size = 0;

  if (3 + scanner->indents.len >=
      TREE_SITTER_SERIALIZATION_BUFFER_SIZE) {
    return 0;
  }

  size_t pending_dedents_length = sizeof(scanner->pending_dedents);
  buffer[size++] = (char) pending_dedents_length;
  if (pending_dedents_length > 0) {
    memcpy( & buffer[size], & scanner->pending_dedents, pending_dedents_length);
  }
  size += pending_dedents_length;

  int iter = 1;
  for (; iter != scanner->indents.len &&
         size < TREE_SITTER_SERIALIZATION_BUFFER_SIZE;
       ++iter) {
    buffer[size++] = (char) scanner->indents.data[iter];
  }

  return size;
}

/**
 * Load another parser state into the currently active state.
 * `payload` is the state of the previous parser execution, while `buffer` is
 * the saved state of a different position (e.g. when doing incremental
 * parsing).
 */
void tree_sitter_aesophia_external_scanner_deserialize(void * payload,
                                                       const char * buffer,
                                                       unsigned length) {
  Scanner * scanner = (Scanner * ) payload;
  VEC_CLEAR(scanner->indents);
  VEC_PUSH(scanner->indents, 0);
  scanner->pending_dedents = 0;

  if (length == 0) {
    return;
  }

  size_t size = 0;

  size_t pending_dedents_length = (unsigned char) buffer[size++];
  if (pending_dedents_length > 0) {
    memcpy( & scanner->pending_dedents, & buffer[size], pending_dedents_length);
    size += pending_dedents_length;
  }

  for (; size < length; size++) {
    VEC_PUSH(scanner->indents, (unsigned char) buffer[size]);
  }
  assert(size == length);
}

/**
 * Destroy the state.
 */
void tree_sitter_aesophia_external_scanner_destroy(void * payload) {
  Scanner * scanner = (Scanner * ) payload;
  VEC_FREE(scanner->indents);
  free(scanner);
}
