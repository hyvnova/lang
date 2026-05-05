#include "tree_sitter/parser.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

enum TokenType {
  PYTHON_CONTENT,
};

static const char *END_SENTINEL = "#[endpython]";
static const uint32_t END_SENTINEL_LEN = 12;

void *tree_sitter_lamp_external_scanner_create(void) {
  return NULL;
}

void tree_sitter_lamp_external_scanner_destroy(void *payload) {
  (void)payload;
}

unsigned tree_sitter_lamp_external_scanner_serialize(void *payload, char *buffer) {
  (void)payload;
  (void)buffer;
  return 0;
}

void tree_sitter_lamp_external_scanner_deserialize(void *payload, const char *buffer, unsigned length) {
  (void)payload;
  (void)buffer;
  (void)length;
}

static bool scan_end_sentinel(TSLexer *lexer) {
  for (uint32_t i = 0; i < END_SENTINEL_LEN; i++) {
    if (lexer->lookahead != END_SENTINEL[i]) {
      return false;
    }
    lexer->advance(lexer, false);
  }
  return true;
}

bool tree_sitter_lamp_external_scanner_scan(void *payload, TSLexer *lexer, const bool *valid_symbols) {
  (void)payload;

  if (!valid_symbols[PYTHON_CONTENT]) {
    return false;
  }

  bool has_content = false;

  while (lexer->lookahead) {
    if (lexer->lookahead == '#') {
      if (scan_end_sentinel(lexer)) {
        return has_content;
      }
      has_content = true;
      lexer->mark_end(lexer);
      continue;
    }

    has_content = true;
    lexer->advance(lexer, false);
    lexer->mark_end(lexer);
  }

  return has_content;
}
