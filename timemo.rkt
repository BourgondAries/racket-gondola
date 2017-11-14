#lang racket

(provide define/timemo)

(require (for-syntax racket/syntax syntax/parse)
         racket/async-channel)

; Timed memoization: memoize a function for a certain amount of time, then refresh its result
; useful for caching filesystem operations.

(define (thread-with-name name proc)
  (thread proc))

(define-syntax (define/timemo stx)
  (syntax-parse stx
    [(_ name:id ((~or
                   (~optional (~seq #:every every) #:defaults ([every #'void]) #:too-many "every is already defined")
                   (~optional (~seq #:once once) #:defaults ([once #'void]) #:too-many "once is already defined")
                   (~optional (~seq #:time time) #:defaults ([time #'1]) #:too-many "time is already defined")
                   (~optional (~seq #:threader threader) #:defaults ([threader #'thread-with-name]) #:too-many "threader is already defined")) ...)
                 thunk:expr ...+)
     #'(define name (let* ([chn (make-async-channel)]
                           [get (lambda _ (let ([result (async-channel-get chn)])
                                            (async-channel-put chn result)
                                            result))]
                           [thr (threader 'name
                                          (lambda ()
                                            once
                                            (every `(name ,(current-seconds)))
                                            (let loop ([result (begin thunk ...)])
                                              (async-channel-put chn result)
                                              (sleep time)
                                              (every `(name ,(current-seconds)))
                                              (let ([new (begin thunk ...)])
                                                (async-channel-get chn)
                                                (loop new)))))])
                      get))]))
