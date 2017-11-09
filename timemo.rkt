#lang racket

(provide define/timemo timemo)

(require (for-syntax racket/syntax syntax/parse)
         racket/async-channel)

; Timed memoization: memoize a function for a certain amount of time, then refresh its result
; useful for caching filesystem operations.

; Call `(name)` to get the cached result. `time` is in seconds and can be any real number.
; `proc` is the procedure that will be run every `time` seconds.
; A custom `threader` can be specified. This is useful when you want to define threads in
; a particular place. An example is the persistent store for the `reloadable` package.
; `once` is run once before the thread loops and is used to set up variables/state.
; The threader must take an identifier and a lambda as its arguments, in that order.

; Example: (timemo threaded-renderer 60 current-seconds)
; Example: (timemo threaded-renderer 60 directory-list (current-directory "/home"))
; Example: (timemo threaded-renderer 60 directory-list (current-directory "/home") reloadable-safe-thread) ; Note, reloadable-safe-thread does not exist in the standard library

; Another way to defer to timemo is to use define/timemo:
; Example (define/timemo name (60 (void)) (displayln "Hello!"))

(define-syntax (timemo stx)
  (syntax-parse stx
    [(self name:id time:expr proc:expr)
     #'(self name time proc (void))]
    [(_ name:id time:expr proc:expr once:expr)
     #'(define name (let* ([chn (make-async-channel)]
                           [get (lambda _ (let ([result (async-channel-get chn)])
                                            (async-channel-put chn result)
                                            result))]
                           [thr (thread (lambda ()
                                          once
                                          (let loop ([result (proc)])
                                            (async-channel-put chn result)
                                            (sleep time)
                                            (let ([new (proc)])
                                              (async-channel-get chn)
                                              (loop new)))))])
                      get))]
    [(_ name:id time:expr proc:expr once:expr threader:id)
     (with-syntax ([channel        (format-id #'name "timemo-internal:~a-channel" #'name)]
                   [channel-thread (format-id #'name "timemo-internal:~a-thread" #'name)])
       #'(begin
         (define name (lambda _
                        (let ([result (async-channel-get channel)])
                          (async-channel-put channel result)
                          result)))
         (define channel (make-async-channel))
         (threader channel-thread (lambda () once
                                             (let loop ([result (proc)])
                                             (async-channel-put channel result)
                                             (sleep time)
                                             (let ([new (proc)])
                                               (async-channel-get channel)
                                               (loop new)))))))]))

(define-syntax (define/timemo stx)
  (syntax-parse stx
    [(_ name:id () thunk:expr ...+)
     #'(timemo name 10 (lambda () thunk ...))]
    [(_ name:id (time:expr) thunk:expr ...+)
     #'(timemo name time (lambda () thunk ...))]
    [(_ name:id (time:expr once:expr) thunk:expr ...+)
     #'(timemo name time (lambda () thunk ...) once)]
    [(_ name:id (time:expr once:expr threader:id) thunk:expr ...+)
     #'(timemo name time (lambda () thunk ...) once threader)]))
