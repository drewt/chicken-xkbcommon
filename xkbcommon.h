/*
 * Copyright 1985, 1987, 1990, 1998  The Open Group
 * Copyright 2008  Dan Nicholson
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * Except as contained in this notice, the names of the authors or their
 * institutions shall not be used in advertising or otherwise to promote the
 * sale, use or other dealings in this Software without prior written
 * authorization from the authors.
 */

/************************************************************
 * Copyright (c) 1993 by Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that the above copyright
 * notice appear in all copies and that both that copyright
 * notice and this permission notice appear in supporting
 * documentation, and that the name of Silicon Graphics not be
 * used in advertising or publicity pertaining to distribution
 * of the software without specific prior written permission.
 * Silicon Graphics makes no representation about the suitability
 * of this software for any purpose. It is provided "as is"
 * without any express or implied warranty.
 *
 * SILICON GRAPHICS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
 * SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE. IN NO EVENT SHALL SILICON
 * GRAPHICS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
 * OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION  WITH
 * THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 ********************************************************/

/*
 * Copyright © 2009-2012 Daniel Stone
 * Copyright © 2012 Intel Corporation
 * Copyright © 2012 Ran Benita
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
 *
 * Author: Daniel Stone <daniel@fooishbar.org>
 */

struct xkb_context;
struct xkb_keymap;
struct xkb_state;
typedef uint32_t xkb_keycode_t;
typedef uint32_t xkb_keysym_t;
typedef uint32_t xkb_layout_index_t;
typedef uint32_t xkb_layout_mask_t;
typedef uint32_t xkb_level_index_t;
typedef uint32_t xkb_mod_index_t;
typedef uint32_t xkb_mod_mask_t;
typedef uint32_t xkb_led_index_t;
typedef uint32_t xkb_led_mask_t;

struct xkb_rule_names {
    const char *rules;
    const char *model;
    const char *layout;
    const char *variant;
    const char *options;
};

int
xkb_keysym_get_name(xkb_keysym_t keysym, char *buffer, size_t size);

xkb_keysym_t
xkb_keysym_from_name(const char *name, enum xkb_keysym_flags flags);

int
xkb_keysym_to_utf8(xkb_keysym_t keysym, char *buffer, size_t size);

uint32_t
xkb_keysym_to_utf32(xkb_keysym_t keysym);

xkb_keysym_t
xkb_keysym_to_upper(xkb_keysym_t ks);

xkb_keysym_t
xkb_keysym_to_lower(xkb_keysym_t ks);

struct xkb_context *
xkb_context_new(enum xkb_context_flags flags);

struct xkb_context *
xkb_context_ref(struct xkb_context *context);

void
xkb_context_unref(struct xkb_context *context);

void
xkb_context_set_user_data(struct xkb_context *context, void *user_data);

void *
xkb_context_get_user_data(struct xkb_context *context);

int
xkb_context_include_path_append(struct xkb_context *context, const char *path);

int
xkb_context_include_path_append_default(struct xkb_context *context);

int
xkb_context_include_path_reset_defaults(struct xkb_context *context);

void
xkb_context_include_path_clear(struct xkb_context *context);

unsigned int
xkb_context_num_include_paths(struct xkb_context *context);

const char *
xkb_context_include_path_get(struct xkb_context *context, unsigned int index);

void
xkb_context_set_log_level(struct xkb_context *context,
                          enum xkb_log_level level);

enum xkb_log_level
xkb_context_get_log_level(struct xkb_context *context);

void
xkb_context_set_log_verbosity(struct xkb_context *context, int verbosity);

int
xkb_context_get_log_verbosity(struct xkb_context *context);

//void
//xkb_context_set_log_fn(struct xkb_context *context,
//                       void (*log_fn)(struct xkb_context *context,
//                                      enum xkb_log_level level,
//                                      const char *format, va_list args));

struct xkb_keymap *
xkb_keymap_new_from_names(struct xkb_context *context,
                          const struct xkb_rule_names *names,
                          enum xkb_keymap_compile_flags flags);

struct xkb_keymap *
xkb_keymap_new_from_file(struct xkb_context *context, FILE *file,
                         enum xkb_keymap_format format,
                         enum xkb_keymap_compile_flags flags);

struct xkb_keymap *
xkb_keymap_new_from_string(struct xkb_context *context, const char *string,
                           enum xkb_keymap_format format,
                           enum xkb_keymap_compile_flags flags);

struct xkb_keymap *
xkb_keymap_new_from_buffer(struct xkb_context *context, const char *buffer,
                           size_t length, enum xkb_keymap_format format,
                           enum xkb_keymap_compile_flags flags);

struct xkb_keymap *
xkb_keymap_ref(struct xkb_keymap *keymap);

void
xkb_keymap_unref(struct xkb_keymap *keymap);

char *
xkb_keymap_get_as_string(struct xkb_keymap *keymap,
                         enum xkb_keymap_format format);

xkb_keycode_t
xkb_keymap_min_keycode(struct xkb_keymap *keymap);

xkb_keycode_t
xkb_keymap_max_keycode(struct xkb_keymap *keymap);

typedef void
(*xkb_keymap_key_iter_t)(struct xkb_keymap *keymap, xkb_keycode_t key,
                         void *data);

void
xkb_keymap_key_for_each(struct xkb_keymap *keymap, xkb_keymap_key_iter_t iter,
                        void *data);

const char *
xkb_keymap_key_get_name(struct xkb_keymap *keymap, xkb_keycode_t key);

xkb_keycode_t
xkb_keymap_key_by_name(struct xkb_keymap *keymap, const char *name);

xkb_mod_index_t
xkb_keymap_num_mods(struct xkb_keymap *keymap);

const char *
xkb_keymap_mod_get_name(struct xkb_keymap *keymap, xkb_mod_index_t idx);

xkb_mod_index_t
xkb_keymap_mod_get_index(struct xkb_keymap *keymap, const char *name);

xkb_layout_index_t
xkb_keymap_num_layouts(struct xkb_keymap *keymap);

const char *
xkb_keymap_layout_get_name(struct xkb_keymap *keymap, xkb_layout_index_t idx);

xkb_layout_index_t
xkb_keymap_layout_get_index(struct xkb_keymap *keymap, const char *name);

xkb_led_index_t
xkb_keymap_num_leds(struct xkb_keymap *keymap);

const char *
xkb_keymap_led_get_name(struct xkb_keymap *keymap, xkb_led_index_t idx);

xkb_led_index_t
xkb_keymap_led_get_index(struct xkb_keymap *keymap, const char *name);

xkb_layout_index_t
xkb_keymap_num_layouts_for_key(struct xkb_keymap *keymap, xkb_keycode_t key);

xkb_level_index_t
xkb_keymap_num_levels_for_key(struct xkb_keymap *keymap, xkb_keycode_t key,
                              xkb_layout_index_t layout);

int
xkb_keymap_key_get_syms_by_level(struct xkb_keymap *keymap,
                                 xkb_keycode_t key,
                                 xkb_layout_index_t layout,
                                 xkb_level_index_t level,
                                 const xkb_keysym_t **syms_out);

int
xkb_keymap_key_repeats(struct xkb_keymap *keymap, xkb_keycode_t key);

struct xkb_state *
xkb_state_new(struct xkb_keymap *keymap);

struct xkb_state *
xkb_state_ref(struct xkb_state *state);

void
xkb_state_unref(struct xkb_state *state);

struct xkb_keymap *
xkb_state_get_keymap(struct xkb_state *state);

enum xkb_state_component
xkb_state_update_key(struct xkb_state *state, xkb_keycode_t key,
                     enum xkb_key_direction direction);

enum xkb_state_component
xkb_state_update_mask(struct xkb_state *state,
                      xkb_mod_mask_t depressed_mods,
                      xkb_mod_mask_t latched_mods,
                      xkb_mod_mask_t locked_mods,
                      xkb_layout_index_t depressed_layout,
                      xkb_layout_index_t latched_layout,
                      xkb_layout_index_t locked_layout);

int
xkb_state_key_get_syms(struct xkb_state *state, xkb_keycode_t key,
                       const xkb_keysym_t **syms_out);

int
xkb_state_key_get_utf8(struct xkb_state *state, xkb_keycode_t key,
                       char *buffer, size_t size);

uint32_t
xkb_state_key_get_utf32(struct xkb_state *state, xkb_keycode_t key);

xkb_keysym_t
xkb_state_key_get_one_sym(struct xkb_state *state, xkb_keycode_t key);

xkb_layout_index_t
xkb_state_key_get_layout(struct xkb_state *state, xkb_keycode_t key);

xkb_level_index_t
xkb_state_key_get_level(struct xkb_state *state, xkb_keycode_t key,
                        xkb_layout_index_t layout);

xkb_mod_mask_t
xkb_state_serialize_mods(struct xkb_state *state,
                         enum xkb_state_component components);

xkb_layout_index_t
xkb_state_serialize_layout(struct xkb_state *state,
                           enum xkb_state_component components);

int
xkb_state_mod_name_is_active(struct xkb_state *state, const char *name,
                             enum xkb_state_component type);

int
xkb_state_mod_index_is_active(struct xkb_state *state, xkb_mod_index_t idx,
                              enum xkb_state_component type);

xkb_mod_mask_t
xkb_state_key_get_consumed_mods2(struct xkb_state *state, xkb_keycode_t key,
                                 enum xkb_consumed_mode mode);

xkb_mod_mask_t
xkb_state_key_get_consumed_mods(struct xkb_state *state, xkb_keycode_t key);

int
xkb_state_mod_index_is_consumed2(struct xkb_state *state,
                                 xkb_keycode_t key,
                                 xkb_mod_index_t idx,
                                 enum xkb_consumed_mode mode);

int
xkb_state_mod_index_is_consumed(struct xkb_state *state, xkb_keycode_t key,
                                xkb_mod_index_t idx);

xkb_mod_mask_t
xkb_state_mod_mask_remove_consumed(struct xkb_state *state, xkb_keycode_t key,
                                   xkb_mod_mask_t mask);

int
xkb_state_layout_name_is_active(struct xkb_state *state, const char *name,
                                enum xkb_state_component type);

int
xkb_state_layout_index_is_active(struct xkb_state *state,
                                 xkb_layout_index_t idx,
                                 enum xkb_state_component type);

int
xkb_state_led_name_is_active(struct xkb_state *state, const char *name);

int
xkb_state_led_index_is_active(struct xkb_state *state, xkb_led_index_t idx);
