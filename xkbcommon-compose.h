/*
 * Copyright © 2013 Ran Benita
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

typedef uint32_t xkb_keysym_t;

struct xkb_compose_table;
struct xkb_compose_state;

struct xkb_compose_table *
xkb_compose_table_new_from_locale(struct xkb_context *context,
                                  const char *locale,
                                  enum xkb_compose_compile_flags flags);

struct xkb_compose_table *
xkb_compose_table_new_from_file(struct xkb_context *context,
                                FILE *file,
                                const char *locale,
                                enum xkb_compose_format format,
                                enum xkb_compose_compile_flags flags);

struct xkb_compose_table *
xkb_compose_table_new_from_buffer(struct xkb_context *context,
                                  const char *buffer, size_t length,
                                  const char *locale,
                                  enum xkb_compose_format format,
                                  enum xkb_compose_compile_flags flags);

struct xkb_compose_table *
xkb_compose_table_ref(struct xkb_compose_table *table);

void
xkb_compose_table_unref(struct xkb_compose_table *table);

struct xkb_compose_state *
xkb_compose_state_new(struct xkb_compose_table *table,
                      enum xkb_compose_state_flags flags);

struct xkb_compose_state *
xkb_compose_state_ref(struct xkb_compose_state *state);

void
xkb_compose_state_unref(struct xkb_compose_state *state);

struct xkb_compose_table *
xkb_compose_state_get_compose_table(struct xkb_compose_state *state);

enum xkb_compose_feed_result
xkb_compose_state_feed(struct xkb_compose_state *state,
                       xkb_keysym_t keysym);

void
xkb_compose_state_reset(struct xkb_compose_state *state);

enum xkb_compose_status
xkb_compose_state_get_status(struct xkb_compose_state *state);

int
xkb_compose_state_get_utf8(struct xkb_compose_state *state,
                           char *buffer, size_t size);

xkb_keysym_t
xkb_compose_state_get_one_sym(struct xkb_compose_state *state);
