#lang racket/base

(require ffi/unsafe
         ffi/unsafe/define)

(provide editline-state
         editline-init
         editline-gets
         editline-gets/bytes
         editline-cursor
         editline-line
         editline-insert-string
         editline-refresh
         editline-bind
         editline-add-function
         history-state
         history-init
         history-add
         history-add/bytes
         history-get-size
         history-delete)

(define libedit (ffi-lib "libedit" '("2" "")))
(define-ffi-definer define-el libedit)

(define-cpointer-type _EditLine)
(define-cpointer-type _History)

(define-cstruct _LineInfoStruct
  ([buffer _pointer]
   [cursor _pointer]
   [lastchar _pointer]))

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
    (define new-bytes
      (if done?
          (reverse bytes)
          (cons (ptr-ref buf-ptr _byte) bytes)))
    (define new-cursor
      (if (ptr-equal? buf-ptr cursor-ptr)
          cursor
          (add1 cursor)))
    (if done?
        (values new-bytes new-cursor)
        (loop (ptr-add buf-ptr 1 _byte)
              new-cursor
              new-bytes))))

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
           (_fun (_EditLine = (editline-state)) _int -> _string)
           #:c-id el_gets)

(define-el editline-gets/bytes
           (_fun (_EditLine = (editline-state)) _int -> _bytes)
           #:c-id el_gets)

(define-el editline-getc
           (_fun (_EditLine = (editline-state))
                 (ch : (_ptr i _byte))
                 -> (code : _int)
                 -> (values code ch))
           #:c-id el_getc)

(define-el editline-push
           (_fun (_EditLine = (editline-state)) _string -> _void)
           #:c-id el_push)

;; TODO: what is the API for el_parse?

(define-syntax-rule (define-el-get (?getter . ?args)
                      ?enum-sym (?type ...) ?result-type)
  (define (?getter . ?args)
    (define c-fun
      (get-ffi-obj "el_get" (_fun _EditLine _el-op ?type ... -> ?result-type)))
    (apply c-fun ?enum-sym ?args)))

(define-syntax-rule (define-el-set (?setter . ?args)
                      ?enum-sym (?type ...) ?result-type)
  (define (?setter . ?args)
    (define c-fun
      (get-ffi-obj "el_set"
                   libedit
                   (_fun _EditLine _el-op ?type ... -> ?result-type)))
    (apply c-fun ?enum-sym ?args)))

(define-el-set (editline-refresh el) 'refresh 
               ((_EditLine = (editline-state)))
               _void)

(define-el-set (editline-bind el key cmd) 'bind
               ((_EditLine = (editline-state)) _string _string)
               _void)

(define-el-set (editline-add-function el name help fn) 'addfn
               ((_EditLine = (editline-state))
                _string _string
                (_fun (_EditLine = (editline-state)) _byte ->  _byte))
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
                 -> (li : _LineInfoStruct)
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

(define (history-init) (history-state (el-history-init)))

(define-syntax-rule (define-el-history (?history ?arg ...)
                      ?enum-sym (?type ...) ?result-type)
  (define (?history ?arg ...)
    (define c-fun
      (get-ffi-obj "history" libedit
                             (_fun (_History = (history-state))
                                   _history-op
                                   (_pointer = #f)
                                   ?type ... -> ?result-type)))
    (c-fun ?enum-sym ?arg ...)))

(define-el-history (history-add str) 'add (_string) _void)

(define (history-add/bytes bstr)
  (history-add (bytes->string/utf-8 bstr)))

(define-el-history (history-delete str) 'delete (_int) _void)

(define-el-history (history-get-size) 'getsize () _int)
