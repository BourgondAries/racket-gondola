#lang racket

(provide (contract-out (reloadable-safe-thread (-> symbol? procedure? void?))))

(require reloadable)

(define (reloadable-safe-thread name proc)
  (let ([state (make-persistent-state name (lambda () #f))])
    (when (thread? (state))
      (kill-thread (state)))
    (void (state (thread proc)))))
