#lang racket

(provide reloadable-safe-thread)

(require (for-syntax racket/syntax syntax/parse) reloadable)

(define-syntax (reloadable-safe-thread stx)
  (syntax-parse stx
    [(_ name:id proc:expr)
     #'(begin
         (let ([name (make-persistent-state 'name (lambda () #f))])
           (when (thread? (name))
             (kill-thread (name)))
           (void (name (thread proc)))))]))
