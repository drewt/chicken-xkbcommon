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

(foreign-declare "#include <xkbcommon/xkbcommon-compose.h>")

(module (xkbcommon compose)
        (xkb-compose-compile/no-flags
         xkb-compose-format/text-v1

         xkb-compose-table-new-from-locale
         xkb-compose-table-new-from-file
         xkb-compose-table-new-from-buffer
         xkb-compose-table-ref
         xkb-compose-table-unref

         xkb-compose-state/no-flags
         xkb-compose-state-new
         xkb-compose-state-ref
         xkb-compose-state-unref
         xkb-compose-state-get-compose-table

         xkb-compose/nothing
         xkb-compose/composing
         xkb-compose/composed
         xkb-compose/cancelled

         xkb-compose-feed/ignored
         xkb-compose-feed/accepted

         xkb-compose-state-feed
         xkb-compose-state-reset
         xkb-compose-state-get-status
         xkb-compose-state-get-utf8
         xkb-compose-state-get-one-sym)

  (import (scheme)
          (chicken foreign)
          (bind))

  (bind-options default-renaming: "")
  (bind-file "xkbcommon-compose.h")

  ; Define a series of foreign values of the same type.
  (define-syntax define-foreign-values
    (syntax-rules ()
      ((define-foreign-values type)
        (begin))
      ((define-foreign-values type (scm-name c-name) . rest)
        (begin
          (define scm-name (foreign-value c-name type))
          (define-foreign-values type . rest)))))

  (define-foreign-values (enum "xkb_compose_compile_flags")
    (xkb-compose-compile/no-flags "XKB_COMPOSE_COMPILE_NO_FLAGS"))

  (define-foreign-values (enum "xkb_compose_format")
    (xkb-compose-format/text-v1 "XKB_COMPOSE_FORMAT_TEXT_V1"))

  (define-foreign-values (enum "xkb_compose_state_flags")
    (xkb-compose-state/no-flags "XKB_COMPOSE_STATE_NO_FLAGS"))

  (define-foreign-values (enum "xkb_compose_status")
    (xkb-compose/nothing   "XKB_COMPOSE_NOTHING")
    (xkb-compose/composing "XKB_COMPOSE_COMPOSING")
    (xkb-compose/composed  "XKB_COMPOSE_COMPOSED")
    (xkb-compose/cancelled "XKB_COMPOSE_CANCELLED"))

  (define-foreign-values (enum "xkb_compose_feed_result")
    (xkb-compose-feed/ignored  "XKB_COMPOSE_FEED_IGNORED")
    (xkb-compose-feed/accepted "XKB_COMPOSE_FEED_ACCEPTED")))
