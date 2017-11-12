#lang racket

(provide define/timemo)

(require (for-syntax racket/syntax syntax/parse)
         racket/async-channel)

; Timed memoization: memoize a function for a certain amount of time, then refresh its result
; useful for caching filesystem operations.

(define-syntax (define/timemo stx)
  (syntax-parse stx
    [(_ name:id ((~or
                   (~optional (~seq #:every every) #:defaults ([every #'void]) #:too-many "every is already defined")
                   (~optional (~seq #:once once) #:defaults ([once #'void]) #:too-many "once is already defined")
                   (~optional (~seq #:time time) #:defaults ([time #'1]) #:too-many "time is already defined")
                   (~optional (~seq #:threader threader) #:too-many "threader is already defined")) ...)
                 thunk:expr ...+)
     (if (attribute threader)
       #'(define name (let ([channel (make-async-channel)])
                        (threader name
                          (lambda ()
                            once
                            (every `(timemo name ,(current-seconds)))
                            (let loop ([result (begin thunk ...)])
                              (async-channel-put channel result)
                              (sleep time)
                              (every `(timemo name ,(current-seconds)))
                              (let ([new (begin thunk ...)])
                                (async-channel-get channel)
                                (loop new)))))
                        (lambda _
                          (let ([result (async-channel-get channel)])
                            (async-channel-put channel result)
                            result))))
       #'(define name (let* ([chn (make-async-channel)]
                             [get (lambda _ (let ([result (async-channel-get chn)])
                                              (async-channel-put chn result)
                                              result))]
                             [thr (thread (lambda ()
                                            once
                                            (let loop ([result (begin thunk ...)])
                                              (async-channel-put chn result)
                                              (sleep time)
                                              (let ([new (begin thunk ...)])
                                                (async-channel-get chn)
                                                (loop new)))))])
                        get)))]))
