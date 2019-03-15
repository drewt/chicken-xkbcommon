;; Copyright 2019 Drew Thoreson
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to
;; deal in the Software without restriction, including without limitation the
;; rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
;; sell copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;; IN THE SOFTWARE.

(foreign-declare "#include <xkbcommon/xkbcommon.h>")

(module (xkbcommon)
        (xkb-mod-name/shift
         xkb-mod-name/caps
         xkb-mod-name/ctrl
         xkb-mod-name/alt
         xkb-mod-name/num
         xkb-mod-name/logo

         xkb-led-name/caps
         xkb-led-name/num
         xkb-led-name/scroll

         xkb-keycode-invalid
         xkb-layout-invalid
         xkb-level-invalid
         xkb-mod-invalid
         xkb-led-invalid
         xkb-keycode-max
         xkb-keycode-is-legal-ext
         xkb-keycode-is-legal-x11

         xkb-rule-names-rules
         xkb-rule-names-model
         xkb-rule-names-layout
         xkb-rule-names-variant
         xkb-rule-names-options

         xkb-keysym-get-name
         
         xkb-keysym/no-flags
         xkb-keysym/case-insensitive
         
         xkb-keysym-from-name
         xkb-keysym-to-utf8
         xkb-keysym-to-utf32
         xkb-keysym-to-upper
         xkb-keysym-to-lower

         xkb-context/no-flags
         xkb-context/no-default-includes
         xkb-context/no-environment-names

         make-xkb-context
         xkb-context-new
         xkb-context-ref
         xkb-context-unref
         xkb-context-set-user-data
         xkb-context-get-user-data
         xkb-context-include-path-append
         xkb-context-include-path-append-default
         xkb-context-include-path-reset-defaults
         xkb-context-include-path-clear
         xkb-context-num-include-paths
         xkb-context-include-path-get

         xkb-log-level/critical
         xkb-log-level/error
         xkb-log-level/warning
         xkb-log-level/info
         xkb-log-level/debug

         xkb-context-set-log-level
         xkb-context-get-log-level
         xkb-context-set-log-verbosity
         xkb-context-get-log-verbosity
         ;xkb-context-set-log-fn ; FIXME: va_list

         make-xkb-keymap
         xkb-keymap-compile/no-flags
         xkb-keymap-new-from-names
         xkb-keymap-format/text-v1
         xkb-keymap-new-from-file
         xkb-keymap-new-from-string
         xkb-keymap-new-from-buffer
         xkb-keymap-ref
         xkb-keymap-unref
         xkb-keymap-format/use-original
         xkb-keymap-get-as-string
         xkb-keymap-min-keycode
         xkb-keymap-max-keycode
         xkb-keymap-key-for-each ; FIXME: scheme iterator
         xkb-keymap-key-get-name
         xkb-keymap-key-by-name
         xkb-keymap-num-mods
         xkb-keymap-mod-get-name
         xkb-keymap-mod-get-index
         xkb-keymap-num-layouts
         xkb-keymap-layout-get-name
         xkb-keymap-layout-get-index
         xkb-keymap-num-leds
         xkb-keymap-led-get-name
         xkb-keymap-led-get-index
         xkb-keymap-num-layouts-for-key
         xkb-keymap-num-levels-for-key
         xkb-keymap-key-get-syms-by-level
         xkb-keymap-key-repeats

         xkb-state-new
         xkb-state-ref
         xkb-state-unref
         xkb-state-get-keymap

         xkb-key/up
         xkb-key/down

         xkb-state/mods-depressed
         xkb-state/mods-latched
         xkb-state/mods-locked
         xkb-state/mods-effective
         xkb-state/layout-depressed
         xkb-state/layout-latched
         xkb-state/layout-locked
         xkb-state/layout-effective
         xkb-state/leds

         xkb-state-update-key
         xkb-state-update-mask
         xkb-state-key-get-syms
         xkb-state-key-get-utf8
         xkb-state-key-get-utf32
         xkb-state-key-get-one-sym
         xkb-state-key-get-layout
         xkb-state-key-get-level
         xkb-state-match/any
         xkb-state-match/all
         xkb-state-match/non-exclusive
         xkb-state-serialize-mods
         xkb-state-serialize-layout
         xkb-state-mod-name-is-active
         ;xkb-state-mod-names-are-active ; FIXME: variadic
         xkb-state-mod-index-is-active
         ;xkb-state-mod-indices-are-active ; FIXME: variadic

         xkb-consumed-mode/xkb
         xkb-consumed-mode/gtk

         xkb-state-key-get-consumed-mods2
         xkb-state-key-get-consumed-mods
         xkb-state-mod-index-is-consumed2
         xkb-state-mod-index-is-consumed
         xkb-state-mod-mask-remove-consumed
         xkb-state-layout-name-is-active
         xkb-state-layout-index-is-active
         xkb-state-led-name-is-active
         xkb-state-led-index-is-active)
  (import (scheme)
          (srfi 1)
          (chicken foreign)
          (bind))

  (define-foreign-type xkb-context* (c-pointer (struct "xkb_context")))
  (define-foreign-type xkb-keymap* (c-pointer (struct "xkb_keymap")))
  (define-foreign-type xkb-rule-names* (c-pointer (struct "xkb_rule_names")))
  (define-foreign-type xkb-state* (c-pointer (struct "xkb_state")))

  (define-foreign-type xkb-keycode unsigned-int32)
  (define-foreign-type xkb-keysym unsigned-int32)

  (bind-rename xkb_state_key_get_syms %xkb-state-key-get-syms)
  (bind-options mutable-fields: #t
                default-renaming: "")
  (bind-file "xkbcommon.h")

  ; Define a series of foreign values of the same type.
  (define-syntax define-foreign-values
    (syntax-rules ()
      ((define-foreign-values type)
        (begin))
      ((define-foreign-values type (scm-name c-name) . rest)
        (begin
          (define scm-name (foreign-value c-name type))
          (define-foreign-values type . rest)))))

  (define-foreign-values (const c-string)
    (xkb-mod-name/shift "XKB_MOD_NAME_SHIFT")
    (xkb-mod-name/caps  "XKB_MOD_NAME_CAPS")
    (xkb-mod-name/ctrl  "XKB_MOD_NAME_CTRL")
    (xkb-mod-name/alt   "XKB_MOD_NAME_ALT")
    (xkb-mod-name/num   "XKB_MOD_NAME_NUM")
    (xkb-mod-name/logo  "XKB_MOD_NAME_LOGO"))

  (define-foreign-values (const c-string)
    (xkb-led-name/caps   "XKB_LED_NAME_CAPS")
    (xkb-led-name/num    "XKB_LED_NAME_NUM")
    (xkb-led-name/scroll "XKB_LED_NAME_SCROLL"))

  (define-foreign-values unsigned-int
    (xkb-keycode-invalid "XKB_KEYCODE_INVALID")
    (xkb-layout-invalid  "XKB_LAYOUT_INVALID")
    (xkb-level-invalid   "XKB_LEVEL_INVALID")
    (xkb-mod-invalid     "XKB_MOD_INVALID")
    (xkb-led-invalid     "XKB_LED_INVALID")
    (xkb-keycode-max     "XKB_KEYCODE_MAX"))

  (define (xkb-keycode-is-legal-ext key)
    (<= key xkb-keycode-max))

  (define (xkb-keycode-is-legal-x11 key)
    (and (>= key 8) (<= key 255)))

  (define-foreign-values (enum "xkb_keysym_flags")
    (xkb-keysym/no-flags         "XKB_KEYSYM_NO_FLAGS")
    (xkb-keysym/case-insensitive "XKB_KEYSYM_CASE_INSENSITIVE"))

  (define-foreign-values (enum "xkb_context_flags")
    (xkb-context/no-flags             "XKB_CONTEXT_NO_FLAGS")
    (xkb-context/no-default-includes  "XKB_CONTEXT_NO_DEFAULT_INCLUDES")
    (xkb-context/no-environment-names "XKB_CONTEXT_NO_ENVIRONMENT_NAMES"))

  (define-foreign-values (enum "xkb_log_level")
    (xkb-log-level/critical "XKB_LOG_LEVEL_CRITICAL")
    (xkb-log-level/error    "XKB_LOG_LEVEL_ERROR")
    (xkb-log-level/warning  "XKB_LOG_LEVEL_WARNING")
    (xkb-log-level/info     "XKB_LOG_LEVEL_INFO")
    (xkb-log-level/debug    "XKB_LOG_LEVEL_DEBUG"))

  (define-foreign-values (enum "xkb_keymap_compile_flags")
    (xkb-keymap-compile/no-flags "XKB_KEYMAP_COMPILE_NO_FLAGS"))

  (define-foreign-values (enum "xkb_keymap_format")
    (xkb-keymap-format/text-v1      "XKB_KEYMAP_FORMAT_TEXT_V1")
    (xkb-keymap-format/use-original "XKB_KEYMAP_USE_ORIGINAL_FORMAT"))

  (define-foreign-values (enum "xkb_key_direction")
    (xkb-key/up   "XKB_KEY_UP")
    (xkb-key/down "XKB_KEY_DOWN"))

  (define-foreign-values (enum "xkb_state_component")
    (xkb-state/mods-depressed   "XKB_STATE_MODS_DEPRESSED")
    (xkb-state/mods-latched     "XKB_STATE_MODS_LATCHED")
    (xkb-state/mods-locked      "XKB_STATE_MODS_LOCKED")
    (xkb-state/mods-effective   "XKB_STATE_MODS_EFFECTIVE")
    (xkb-state/layout-depressed "XKB_STATE_LAYOUT_DEPRESSED")
    (xkb-state/layout-latched   "XKB_STATE_LAYOUT_LATCHED")
    (xkb-state/layout-locked    "XKB_STATE_LAYOUT_LOCKED")
    (xkb-state/layout-effective "XKB_STATE_LAYOUT_EFFECTIVE")
    (xkb-state/leds             "XKB_STATE_LEDS"))

  (define-foreign-values (enum "xkb_state_match")
    (xkb-state-match/any           "XKB_STATE_MATCH_ANY")
    (xkb-state-match/all           "XKB_STATE_MATCH_ALL")
    (xkb-state-match/non-exclusive "XKB_STATE_MATCH_NON_EXCLUSIVE"))

  (define-foreign-values (enum "xkb_consumed_mode")
    (xkb-consumed-mode/xkb "XKB_CONSUMED_MODE_XKB")
    (xkb-consumed-mode/gtk "XKB_CONSUMED_MODE_GTK"))

  (define make-xkb-context
    (foreign-lambda* xkb-context* ()
      "C_return(xkb_context_new(XKB_CONTEXT_NO_FLAGS));"))

  (define make-xkb-keymap
    (foreign-lambda* xkb-keymap* ((xkb-context* context))
      "struct xkb_rule_names rules = { 0 };"
      "C_return(xkb_map_new_from_names(context, &rules, XKB_KEYMAP_COMPILE_NO_FLAGS));"))

  (define (syms->list n syms)
    (map (lambda (i)
           ((foreign-lambda* xkb-keysym ((int i) ((c-pointer xkb-keysym) syms))
              "C_return(syms[i]);")
            i syms))
         (iota n)))

  (define (xkb-state-key-get-syms state keycode)
    (let-location ((syms (c-pointer xkb-keysym)))
      (let ((n (%xkb-state-key-get-syms state keycode (location syms))))
        (syms->list n syms))))

  (define xkb-keysym-get-name
    (foreign-lambda* c-string* ((unsigned-int32 keysym))
      "char *buffer = malloc(128);"
      "xkb_keysym_get_name(keysym, buffer, 128);"
      "C_return(buffer);")))
