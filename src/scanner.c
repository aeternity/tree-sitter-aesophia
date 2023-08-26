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

#define VEC_REVERSE(vec)                                                       \
    do {                                                                       \
        if ((vec).len > 1) {                                                   \
            for (size_t i = 0, j = (vec).len - 1; i < j; i++, j--) {           \
                uint8_t tmp = (vec).data[i];                                   \
                (vec).data[i] = (vec).data[j];                                 \
                (vec).data[j] = tmp;                                           \
            }                                                                  \
        }                                                                      \
    } while (0)

enum TokenType {
  BLOCK_OPEN_INLINE,
  BLOCK_OPEN,
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
  uint32_t indent_length;
  vec indents;
  vec runback;
}
Scanner;


#ifdef DEBUG
static void print_scanner(Scanner* scanner) {
  printf("INDENTS:");
  for(size_t i = 0; i < scanner->indents.len; ++i) {
    printf(" %d", scanner->indents.data[i]);
  }
  printf("\nRUNBACK:");
  for(size_t i = 0; i < scanner->runback.len; ++i) {
    if(scanner->runback.data[i]) {
      printf("C");
    } else {
      printf("S");
    }
  }
  printf("\nINDENT: %d\n", scanner->indent_length);
}

static void print_symbols(const bool *valid_symbols) {
  if(valid_symbols[ERROR_STATE]) {
    printf("ERROR STATE\n");
  } else {
    printf("VALID SYMBOLS:\n");
    printf("    BLOCK_OPEN_INLINE: %d\n", valid_symbols[BLOCK_OPEN_INLINE]);
    printf("    BLOCK_OPEN: %d\n", valid_symbols[BLOCK_OPEN]);
    printf("    BLOCK_SEMI: %d\n", valid_symbols[BLOCK_SEMI]);
    printf("    BLOCK_CLOSE: %d\n", valid_symbols[BLOCK_CLOSE]);
    printf("    BLOCK_COMMENT_CONTENT: %d\n", valid_symbols[BLOCK_COMMENT_CONTENT]);
    printf("    ERROR_STAT: %d\n", valid_symbols[ERROR_STATE]);
  }
}

static void print_symbol(int t) {
  switch(t) {
  case BLOCK_OPEN_INLINE:
    printf("BLOCK_OPEN_INLINE");
    break;
  case BLOCK_OPEN:
    printf("BLOCK_OPEN");
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
    printf("DID NOT OPEN /\n");
    goto NOT_ACCEPT;
  }
  advance(lexer);
  if (lexer->lookahead != '*') {
    printf("DID NOT OPEN *\n");
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
      printf("FAIL DUE TO ERROR STATE\n");
      goto NOT_ACCEPT;
    }

    // First handle eventual runback tokens, we saved on a previous scan op
    if (scanner->runback.len > 0 && VEC_BACK(scanner->runback) == 0 &&
        valid_symbols[BLOCK_SEMI]) {
        VEC_POP(scanner->runback);
        lexer->result_symbol = BLOCK_SEMI;
        printf("RUNBACK SEMI\n");
        goto ACCEPT;
    }
    if (scanner->runback.len > 0 && VEC_BACK(scanner->runback) == 1 &&
        valid_symbols[BLOCK_CLOSE]) {
        VEC_POP(scanner->runback);
        lexer->result_symbol = BLOCK_CLOSE;
        printf("RUNBACK CLOSE");
        goto ACCEPT;
    }
    VEC_CLEAR(scanner->runback);

    // Check if we have newlines and how much indentation
    bool has_line_end = false;
    bool has_space = false;
    bool can_call_mark_end = true;
    lexer->mark_end(lexer);

    // Skip to the first line with something meaningful
    while(true) {
      if (lexer->lookahead == ' ' || lexer->lookahead == '\r') {
        // Skip all whitespaces
        has_space = true;
        skip(lexer);
      } else if (lexer->lookahead == '\t') {
        // No tabs
        printf("FOUND TAB\n");
        goto NOT_ACCEPT;
      } else if (lexer->lookahead == '\n') {
        // Calculate indent
        skip(lexer);
        has_line_end = true;
        has_space = false;
        while(true) {
          if (lexer->lookahead == ' ') {
            has_space = true;
            skip(lexer);
          } else {
            scanner->indent_length = lexer->get_column(lexer);
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
          printf("NOT A LINE COMMENT\n");
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
          printf("NOT A BLOCK COMMENT\n");
          goto NOT_ACCEPT;
        }
      } else if (lexer->eof(lexer)) {
        if (valid_symbols[BLOCK_CLOSE]) {
          lexer->result_symbol = BLOCK_CLOSE;
          goto ACCEPT;
        }
        if (valid_symbols[BLOCK_SEMI]) {
          lexer->result_symbol = BLOCK_SEMI;
          goto ACCEPT;
        }
        if (valid_symbols[BLOCK_COMMENT_CONTENT]) {
          lexer->result_symbol = BLOCK_COMMENT_CONTENT;
          goto ACCEPT;
        }
        break;
      } else {
        break;
      }
    }

    printf("SCANNED: NL=%d, WS=%d, EOF=%d, indent=%d\n", has_line_end, has_space, lexer->eof(lexer), scanner->indent_length);
    if (valid_symbols[BLOCK_COMMENT_CONTENT]) {
        if (!can_call_mark_end) {
          printf("CAN'T CALL MARK END\n");
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

    if (valid_symbols[BLOCK_OPEN_INLINE] &&
        !lexer->eof(lexer) &&
        !has_line_end &&
        has_space
        ) {
      VEC_PUSH(scanner->indents, lexer->get_column(lexer));
      lexer->result_symbol = BLOCK_OPEN_INLINE;
      lexer->mark_end(lexer);
      goto ACCEPT;
    }

    if (has_line_end) {
      if (scanner->indent_length > VEC_BACK(scanner->indents) &&
          valid_symbols[BLOCK_OPEN] && !lexer->eof(lexer)) {
        VEC_PUSH(scanner->indents, lexer->get_column(lexer));
        lexer->result_symbol = BLOCK_OPEN;
        goto ACCEPT;
      }

      while (scanner->indent_length <= VEC_BACK(scanner->indents)) {
        if (scanner->indent_length == VEC_BACK(scanner->indents)) {
          /* // Don't insert VIRTUAL_END_DECL when there is a line */
          /* // comment incoming */
          /* if (lexer->lookahead == '-') { */
          /*   skip(lexer); */
          /*   if (lexer->lookahead == '-') { */
          /*     break; */
          /*   } */
          /* } */
          /* // Don't insert VIRTUAL_END_DECL when there is a block */
          /* // comment incoming */
          /* if (lexer->lookahead == '{') { */
          /*   skip(lexer); */
          /*   if (lexer->lookahead == '-') { */
          /*     break; */
          /*   } */
          /* } */
          VEC_PUSH(scanner->runback, 0);
          break;
        }
        if (scanner->indent_length < VEC_BACK(scanner->indents)) {
          VEC_POP(scanner->indents);
          VEC_PUSH(scanner->runback, 1);
        }
      }

      // Our list is the wrong way around, reverse it
      VEC_REVERSE(scanner->runback);
      // Handle the first runback token if we have them, if there are more
      // they will be handled on the next scan operation
      if (scanner->runback.len > 0 && VEC_BACK(scanner->runback) == 0 &&
          valid_symbols[BLOCK_SEMI]) {
        VEC_POP(scanner->runback);
        lexer->result_symbol = BLOCK_SEMI;
        goto ACCEPT;
      }
      if (scanner->runback.len > 0 && VEC_BACK(scanner->runback) == 1 &&
          valid_symbols[BLOCK_CLOSE]) {
        VEC_POP(scanner->runback);
        lexer->result_symbol = BLOCK_CLOSE;
        goto ACCEPT;
      }
      if (lexer->eof(lexer) && valid_symbols[BLOCK_CLOSE]) {
        lexer->result_symbol = BLOCK_CLOSE;
        goto ACCEPT;
      }
    }

 NOT_ACCEPT:
    printf("NOT ACCEPT\n\n");
    return false;
 ACCEPT:
    printf("ACCEPT AT %d: ", lexer->get_column(lexer));
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
  Scanner *scanner = (Scanner *)payload;
    size_t size = 0;

    if (3 + scanner->indents.len + scanner->runback.len >=
        TREE_SITTER_SERIALIZATION_BUFFER_SIZE) {
        return 0;
    }

    size_t runback_count = scanner->runback.len;
    if (runback_count > UINT8_MAX) {
        runback_count = UINT8_MAX;
    }
    buffer[size++] = (char)runback_count;

    if (runback_count > 0) {
        memcpy(&buffer[size], scanner->runback.data, runback_count);
    }
    size += runback_count;

    size_t indent_length_length = sizeof(scanner->indent_length);
    buffer[size++] = (char)indent_length_length;
    if (indent_length_length > 0) {
        memcpy(&buffer[size], &scanner->indent_length, indent_length_length);
    }
    size += indent_length_length;

    int iter = 1;
    for (; iter != scanner->indents.len &&
           size < TREE_SITTER_SERIALIZATION_BUFFER_SIZE;
         ++iter) {
        buffer[size++] = (char)scanner->indents.data[iter];
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
  Scanner *scanner = (Scanner *)payload;
    VEC_CLEAR(scanner->runback);
    VEC_CLEAR(scanner->indents);
    VEC_PUSH(scanner->indents, 0);

    if (length == 0) {
        return;
    }

    size_t size = 0;
    size_t runback_count = (unsigned char)buffer[size++];
    VEC_GROW(scanner->runback, runback_count)
    if (runback_count > 0) {
        memcpy(scanner->runback.data, &buffer[size], runback_count);
        scanner->runback.len = runback_count;
        size += runback_count;
    }

    size_t indent_length_length = (unsigned char)buffer[size++];
    if (indent_length_length > 0) {
        memcpy(&scanner->indent_length, &buffer[size], indent_length_length);
        size += indent_length_length;
    }

    for (; size < length; size++) {
        VEC_PUSH(scanner->indents, (unsigned char)buffer[size]);
    }
    assert(size == length);
}

/**
 * Destroy the state.
 */
void tree_sitter_aesophia_external_scanner_destroy(void *payload) {
    Scanner *scanner = (Scanner *)payload;
    VEC_FREE(scanner->indents);
    VEC_FREE(scanner->runback);
    free(scanner);
}
