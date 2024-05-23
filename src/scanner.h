#ifndef __AESOPHIA_TS_SCANNER
#define __AESOPHIA_TS_SCANNER

#include <stdint.h>
#include <stdbool.h>
#include "tree_sitter/parser.h"


void *tree_sitter_aesophia_external_scanner_create(void);
void tree_sitter_aesophia_external_scanner_destroy(void *payload);
unsigned tree_sitter_aesophia_external_scanner_serialize(void *payload,
                                                         uint8_t *buffer);
void tree_sitter_aesophia_external_scanner_deserialize(void *payload,
                                                       const uint8_t *buffer,
                                                       unsigned length);
bool tree_sitter_aesophia_external_scanner_scan(void *payload, TSLexer *lexer,
                                                const bool *valid_tokens);

#endif // __AESOPHIA_TS_SCANNER
