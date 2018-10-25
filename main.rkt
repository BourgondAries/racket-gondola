#! /usr/bin/env racket
#lang racket/base

(require reloadable
         web-server/servlet
         web-server/servlet-env)

(define blog-dispatch (reloadable-entry-point->procedure
  (make-reloadable-entry-point 'blog-dispatch "handler.rkt")))
(define file-not-found (reloadable-entry-point->procedure
  (make-reloadable-entry-point 'file-not-found "handler.rkt")))

(define (start req)
  (reload!)
  (blog-dispatch req))

(serve/servlet start
  #:stateless? #t
  #:listen-ip #f
  #:port 8000
  #:server-root-path (current-directory)
  #:servlet-regexp #px"^/[^/]*$"
  #:command-line? #t
  #:file-not-found-responder file-not-found
  #:ssl? #f
  )
