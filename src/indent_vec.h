#ifndef __AESOPHIA_TS_INDENT_VEC
#define __AESOPHIA_TS_INDENT_VEC

#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#include "tree_sitter/alloc.h"
#include "tree_sitter/parser.h"

#include "preamble.h"

typedef uint8_t indent_value;

static const indent_value INVALID_INDENT_VALUE = (indent_value)~0U;

struct indent_vec {
  int32_t len;
  int32_t capacity;
  indent_value *data;
};

_nonnull_(1) static void indent_vec_destroy(struct indent_vec *self) {
  ts_free(self->data);
  memset(self, 0, sizeof(*self));
}

_nonnull_(1) static _returns_nonnull_ indent_value *indent_vec_at(
    struct indent_vec *self, int32_t idx) {
  RUNTIME_ASSERT(idx >= 0 && idx < self->len);
  return &self->data[idx];
}

_nonnull_(1) _returns_nonnull_ static _const_ indent_value *indent_vec_at_const(
    const struct indent_vec *self, int32_t idx) {
  RUNTIME_ASSERT(idx >= 0 && idx < self->len);
  return &self->data[idx];
}

_nonnull_(1) static _const_ indent_value
    indent_vec_get(const struct indent_vec *self, int32_t idx) {
  return *indent_vec_at_const((const struct indent_vec *)self, idx);
}

_nonnull_(1) _returns_nonnull_
    static indent_value *indent_vec_at_capacity(struct indent_vec *self,
                                                int32_t idx) {
  RUNTIME_ASSERT(idx >= 0 && idx < self->capacity);
  return &self->data[idx];
}

_nonnull_(1) static int indent_vec_set_capacity(struct indent_vec *self,
                                                int32_t size) {
  if (size < 0) {
    return -1;
  }
  if (size != self->capacity) {
    indent_value *new_data = ts_realloc(self->data, (size_t)size);
    if (!new_data) {
      return -1;
    }
    self->data = new_data;
    self->capacity = size;
    self->len = MIN(self->len, size);
    for (int i = self->len; i < size; i++) {
      *indent_vec_at_capacity(self, i) = INVALID_INDENT_VALUE;
    }
  }
  return 0;
}

_nonnull_(1) static int indent_vec_set_len(struct indent_vec *self,
                                           int32_t size) {
  if (size < 0) {
    return -1;
  }
  if (size > self->capacity) {
    if (indent_vec_set_capacity(self, size) < 0) {
      return -1;
    }
  }

  for (int i = self->len; i < size; i++) {
    *indent_vec_at_capacity(self, i) = INVALID_INDENT_VALUE;
  }
  self->len = size;

  return 0;
}

_nonnull_(1) static int indent_vec_push(struct indent_vec *self,
                                        indent_value value) {
  if (self->len >= self->capacity) {
    int32_t new_capacity = self->len >= 2 ? self->len * 3 / 2 : self->len + 1;
    if (indent_vec_set_capacity(self, new_capacity) < 0) {
      return -1;
    }
  }

  self->len++;
  *indent_vec_at(self, self->len - 1) = value;

  return 0;
}

_nonnull_(1) static void indent_vec_pop(struct indent_vec *self) {
  indent_vec_set_len(self, MAX(0, self->len - 1));
}

_nonnull_(1) static indent_value
    indent_vec_back(const struct indent_vec *self) {
  return indent_vec_get(self, self->len - 1);
}

_nonnull_(1,
          2) static unsigned indent_vec_serialize(const struct indent_vec *self,
                                                  uint8_t *buffer,
                                                  unsigned buffer_len) {
  unsigned n_bytes = (unsigned)self->len * sizeof(*self->data);
  unsigned serialize_len = MIN(buffer_len, n_bytes);

  if (n_bytes > buffer_len) {
    DBG_F("warning: buffer is smaller than vector (%u < %zd), partially "
          "serializing",
          buffer_len, n_bytes);
  }

  // Prevents passing NULL pointer to memcpy
  if (n_bytes == 0) {
    return n_bytes;
  }

  memcpy(buffer, self->data, n_bytes);
  return serialize_len;
}

_nonnull_(1, 2) static void indent_vec_deserialize(struct indent_vec *self,
                                                   const uint8_t *buffer,
                                                   unsigned buffer_len) {
  int32_t n_items = (int32_t)MIN(buffer_len / sizeof(*self->data), INT32_MAX);
  if (indent_vec_set_len(self, n_items) < 0) {
    DBG("cannot deserialize: set_len failed");
    return;
  }
  if (n_items > 0) {
    memcpy(self->data, buffer, (size_t)n_items * sizeof(*self->data));
  }
}

_nonnull_(1) static void indent_vec_debug(const struct indent_vec *self) {
  if (debug_mode) {
    DBG("current layout stack: [");
    for (int32_t i = 0; i < self->len; i++) {
      (void)dprintf(" %" PRIu8, indent_vec_get(self, i));
    }
    (void)dprintf(" ]\n");
  }
}

#endif // __AESOPHIA_TS_VEC
