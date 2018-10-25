#lang racket

(provide (all-defined-out))

(require (for-syntax syntax/parse))

(define-syntax (defines stx)
  (syntax-parse stx
    [(_ (name:expr value:expr) ...)
     #'(begin (define name value) ...)]))

(defines (default-video "/FrontPage.webm")
         (disqus-site "http://gondola.stravers.net")
         (plurality "Gondolas")
         (plurality-normal "Gondolas")
         (list-title "GondolaArchive")
         (singular "Gondola")
         (singular-normal "Gondola")
         (description "Gondola webms depicting our favorite silent observer")
         (forum-name "evo-1"))
