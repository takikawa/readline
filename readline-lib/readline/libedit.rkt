#lang racket/base

(require ffi/unsafe
         ffi/unsafe/define)

(provide editline-state
         editline-init
         editline-gets
         editline-gets/bytes
         editline-cursor
         editline-line
         editline-push
         editline-insert-string
         editline-set-prompt
         editline-set-prompt-esc
         editline-set-editor
         editline-refresh
         editline-bind
         editline-add-function
         history-state
         history-init
         history-add
         history-add/bytes
         history-append
         history-enter
         history-enter/bytes
         history-get-size
         history-set-size
         history-delete)

(define libedit (ffi-lib "libedit" '("2" "")))
(define-ffi-definer define-el libedit)

(define-cpointer-type _EditLine)
(define-cpointer-type _History)

(define-cstruct _LineInfoStruct
  ([buffer _pointer]
   [cursor _pointer]
   [lastchar _pointer]))

(define-cstruct _HistEvent
  ([num _int]
   [str _string]))

;; operations for el_set/el_get
(define _el-op
  (_enum '(prompt = 0
           terminal
           editor
           signal
           bind
           telltc
           settc
           echotc
           setty
           addfn
           hist
           editmode
           rprompt
           getcfn
           clientdata
           unbuffered
           prep-term
           gettc
           getfp
           setfp
           refresh
           prompt-esc
           rprompt-esc
           resize
           alias-text)))

(define _history-op
  (_enum '(func = 0
           setsize
           getsize
           first
           last
           prev
           next
           curr
           set
           add
           enter
           append
           end
           next_str
           prev_str
           next_event
           prev_event
           load
           save
           clear
           setunique
           getunique
           del
           next_evdata
           deldata
           replace
           save_fp)))

;; Convert a _LineInfoStruct to values representing the buffer
;; and the cursor position in the buffer
(define (li->values li)
  (define buf-ptr (LineInfoStruct-buffer li))
  (define cursor-ptr (LineInfoStruct-cursor li))
  (define lastchar-ptr (LineInfoStruct-lastchar li))
  (let loop ([buf-ptr buf-ptr]
             [cursor 0]
             [bytes null])
    (define done? (ptr-equal? buf-ptr lastchar-ptr))
    (define new-cursor
      (if (ptr-equal? buf-ptr cursor-ptr)
          cursor
          (add1 cursor)))
    (cond [done?
           (values (reverse bytes) new-cursor)]
          [else
           (loop (ptr-add buf-ptr 1 _byte)
                 new-cursor
                 (cons (ptr-ref buf-ptr _byte) bytes))])))

;; TODO: what to do about wide char functions?

(define editline-state (make-parameter #f))

(define-el el-init
           (_fun _string _pointer _pointer _pointer -> _EditLine)
           #:c-id el_init)

(define (editline-init prog)
  (define stdin (get-ffi-obj 'stdin #f _pointer))
  (define stdout (get-ffi-obj 'stdout #f _pointer))
  (define stderr (get-ffi-obj 'stderr #f _pointer))
  (editline-state (el-init prog stdin stdout stderr)))

(define-el editline-end
           (_fun (_EditLine = (editline-state)) -> _void)
           #:c-id el_end)

(define-el editline-reset
           (_fun (_EditLine = (editline-state)) -> _void)
           #:c-id el_reset)

(define-el editline-gets
           (_fun (_EditLine = (editline-state))
                 (count : (_ptr o _int))
                 -> _string)
           #:c-id el_gets)

(define-el editline-gets/bytes
           (_fun (_EditLine = (editline-state))
                 (count : (_ptr o _int))
                 -> _bytes)
           #:c-id el_gets)

(define-el editline-getc
           (_fun (_EditLine = (editline-state))
                 (ch : (_ptr o _byte))
                 -> (code : _int)
                 -> (values code ch))
           #:c-id el_getc)

(define-el editline-push
           (_fun (_EditLine = (editline-state)) _string -> _void)
           #:c-id el_push)

;; TODO: what is the API for el_parse?

(define-syntax-rule (define-el-get (?getter ?arg ...)
                      ?enum-sym (?type ...) ?result-type)
  (define (?getter ?arg ...)
    (define c-fun
      (get-ffi-obj "el_get"
                   libedit
                   (_fun (_EditLine = (editline-state))
                         (_el-op = ?enum-sym)
                         ?type ... -> ?result-type)))
    (c-fun ?arg ...)))

(define-syntax-rule (define-el-set (?setter ?arg ...)
                      ?enum-sym (?type ...) ?result-type)
  (define (?setter ?arg ...)
    (define c-fun
      (get-ffi-obj "el_set"
                   libedit
                   (_fun (_EditLine = (editline-state))
                         (_el-op = ?enum-sym)
                         ?type ... -> ?result-type)))
    (c-fun ?arg ...)))

;; For internal use, since customizing this at the Racket level does not
;; seem like something that is really desirable
(define-el-set (el-set-history hist-proc hist)
               'hist (_pointer _History) _void)

(define-el-set (editline-set-prompt p) 'prompt
               [(_cprocedure (list _EditLine) _string
                             #:wrapper (λ (p) (λ (el) (p))))]
               _void)

(define-el-set (editline-set-prompt-esc p ch) 'prompt-esc
               [(_cprocedure (list _EditLine) _string
                             #:wrapper (λ (p) (λ (el) (p))))
                _int]
               _void)

(define-el-set (editline-set-editor ed) 'editor (_string) _void)

(define-el-set (editline-refresh) 'refresh () _void)

(define-el-set (editline-bind key cmd) 'bind
               (_string _string (_pointer = #f)) _void)

(define-el-set (editline-add-function name help fn) 'addfn
               [_string _string
                (_cprocedure (list _EditLine _byte) _byte
                             #:wrapper (λ (p) (λ (el b) (p b))))]
               _void)

(define-el editline-source
           (_fun (_EditLine = (editline-state)) _string -> _int)
           #:c-id el_source)

(define-el editline-resize
           (_fun (_EditLine = (editline-state)) -> _void)
           #:c-id el_resize)

(define-el editline-cursor
           (_fun (_EditLine = (editline-state)) _int -> _int)
           #:c-id el_cursor)

(define-el editline-line
           (_fun (_EditLine = (editline-state))
                 -> (li : _LineInfoStruct-pointer)
                 -> (li->values li))
           #:c-id el_line)

(define-el editline-insert-string
           (_fun (_EditLine = (editline-state)) _string -> _int)
           #:c-id el_line)

;; Track history in its own parameter
(define history-state (make-parameter #f))

(define-el el-history-init
           (_fun -> _History)
           #:c-id history_init)

(define (history-init)
  (define state (el-history-init))
  (history-state state)
  ;; set history management to default history function
  (el-set-history (get-ffi-obj "history" libedit _fpointer) state))

(define-el history-end
           (_fun (_History = (history-state)) -> _void)
           #:c-id history_end)

(define-syntax-rule (define-el-history (?history ?arg ...)
                      ?enum-sym (?type ...) ?result-type
                      ?wrap)
  (define (?history ?arg ...)
    (define c-fun
      (get-ffi-obj "history" libedit
                             (_fun (_History = (history-state))
                                   (he : (_ptr o _HistEvent))
                                   (_history-op = ?enum-sym)
                                   ?type ...
                                   -> (result : ?result-type)
                                   -> (values he result))))
    (define-values (he result) (c-fun ?arg ...))
    (?wrap he result)))

(define-el-history (history-add str) 'add (_string) _void void)
(define-el-history (history-add/bytes str) 'add (_bytes) _void void)

(define-el-history (history-append str) 'append (_string) _void void)

(define-el-history (history-enter str) 'enter (_string) _void void)
(define-el-history (history-enter/bytes str) 'enter (_bytes) _void void)

;; FIXME: make sure this doesn't leak memory due to the HistEvent
(define-el-history (history-delete str) 'del (_int) _void void)

(define-el-history (history-get-size) 'getsize () _int
                   (λ (he r) (HistEvent-num he)))

(define-el-history (history-set-size size) 'setsize (_int) _void void)
