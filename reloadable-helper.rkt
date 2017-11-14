#lang racket

(provide reloadable-safe-thread)

(require reloadable)

(define (reloadable-safe-thread name proc)
  (let ([state (make-persistent-state name (lambda () #f))])
    (when (thread? (state))
      (kill-thread (state)))
    (void (state (thread proc)))))
