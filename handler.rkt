#lang racket

(provide blog-dispatch file-not-found)

(require (for-syntax racket/list racket/pretty racket/syntax syntax/parse)
         "logger.rkt"
         "reloadable-helper.rkt"
         "settings.rkt"
         "timemo.rkt"
         racket/date
         web-server/dispatch
         web-server/servlet
         web-server/servlet-env)

(define common-header
  '((meta ([charset "UTF-8"]))
    (meta ([name "viewport"] [content "width=device-width,maximum-scale=1,minimum-scale=1"]))
    (link ([rel "icon"] [type "image/png"] [href "/images/musings_symbol_16.png"]))
    (link ([rel "icon"] [type "image/png"] [href "/images/musings_symbol_32.png"]))
    (link ([rel "icon"] [type "image/png"] [href "/images/musings_symbol_64.png"]))
    (link ([rel "stylesheet"] [type "text/css"] [href "/css/reset.css"]))
    (link ([rel "stylesheet"] [type "text/css"] [href "/css/style.css?x=8"]))))

(define (serve-index req)
  (serve-post req default-video))

(define (select-random lst)
  (let ([len (length lst)])
    (list-ref lst (random len))))

(define/timemo get-all-webm (120 (current-directory "htdocs") reloadable-safe-thread trce*)
  (map path->string (directory-list "video")))

(define (increment-webm-view-counter webm)
  (let ([target (build-path "statistics" webm)])
    (call-with-file-lock/timeout target 'exclusive
      (lambda ()
        (if (file-exists? target)
          (with-input-from-file target
            (lambda ()
              (let ([increment (add1 (string->number (read-line)))])
                (info^ `("incremented view counter" ,webm ,increment))
                (with-output-to-file target
                  (lambda ()
                    (write increment)
                    (number->string increment)) #:exists 'truncate))))
          (with-output-to-file target
            (lambda ()
              (warn^ `("created new view counter" ,webm))
              (write 1)
              "1")
            #:exists 'truncate)))
      (lambda ()
        (warn^ `("Unable to lock view count" ,webm))
        "N/A"))))

(define (get-random-webm)
  (select-random (get-all-webm)))

(define (get-webm-view-count webm)
  (let ([target (build-path "statistics" webm)])
    (call-with-file-lock/timeout target 'exclusive
                                 (lambda ()
                                   (if (file-exists? target)
                                     (let ([value (with-input-from-file target read-line)])
                                       (if (eof-object? value)
                                         "Corrupted file"
                                         value))
                                     "0"))
                                 (lambda ()
                                   (warn^ `("Unable to lock view count" ,webm))
                                   "N/A"))))

(define/timemo count-webms (300 (void) reloadable-safe-thread trce*)
  (number->string (length (get-all-webm))))

(define (get-random-page)
  (get-random-webm))

(define (get-random-page-raw)
  (string-append "/video/" (get-random-webm)))

(define (strip-extension-webm str)
  (string-trim str ".webm" #:left? false))

(define (post-source post)
  (let ([target (build-path "sources" post)])
    (call-with-file-lock/timeout target 'exclusive
      (lambda ()
        (if (file-exists? target)
          (with-input-from-file target
            (lambda () (read-line)))
          ""))
      (lambda ()
        (warn^ `("Unable to get source" ,post))
        "N/A"))))

(define (post-source-display post)
  (let ([src (post-source post)])
    (if (string=? src "")
      "Unknown (let me know in the comments)"
      src)))

(define (find-next-post post)
  (let* ([all-webm (get-all-webm)]
         [trail    (member post (get-all-webm))])
    (if trail
      (first (if (empty? (rest trail)) all-webm (rest trail)))
      "random")))

(define cookie-autoplay "autoplay")
(define cookie-autoplay-random "random")

(define play-next (make-cookie cookie-autoplay "next"))
(define play-random (make-cookie cookie-autoplay cookie-autoplay-random))

(define (serve-next req)
  (let* ([result (findf
                  (lambda (x)
                    (if (pair? x)
                      (symbol=? (car x) 'v)
                      #f))
                  (request-bindings req))]
         [next (if result (cdr result) "/")])
    (info+ next)
    (redirect-to next #:headers (list (cookie->header play-next)) temporarily)))


(define (get-autoplay-cookie req)
  (let ([result (findf
                  (lambda (x) (string=? (client-cookie-name x) cookie-autoplay))
                  (request-cookies req))])
    (if result
      (string=? (client-cookie-value result) cookie-autoplay-random)
      #t)))

(define (serve-post req post)
  (trce+ req)
  (if (not (file-exists? (string-append "video/" post)))
    (begin
      (erro^ "unable to find video, serving random")
      (redirect-to (get-random-page)))
    (response/xexpr
      #:preamble #"<!DOCTYPE html>"
      `(html
        (head
          ,@common-header
          (script ([type "text/javascript"])
                  "var random_url = \"" ,(blog-url redirect-random) "\";"
                  "var next_url = \"" ,(blog-url serve-next) "\";"
                  "var next = \"" ,(find-next-post post) "\";"
                  "var play_random = " ,(if (get-autoplay-cookie req) "true" "false") ";")
          (title ,(strip-extension-webm post))
          (body ([class "blog"])
                (div ([class "video"])
                     (video ([id "video"] [width "100%"] [height "100%"] [onclick "toggle_pause();"] [autoplay ""] [controls ""])
                            (source ([src ,(string-append "/video/" post)] [type "video/webm"]))))
                (script ([type "text/javascript"] [src "js/video.js?x=3"]))
                (div ([class "bottom"])
                     (a ([class "button"] [href ,(blog-url redirect-random)]) (div ([class "center"]) (span ([class "small"]) "Source: " (br) ,(post-source-display post)) (br) "Next (random)" ,@(if (get-autoplay-cookie req) '((br) (span ([class "autoplay"]) "autoplaying random")) null)))
                     (a ([class "button"] [href ,(string-append "/next?v=" (find-next-post post))]) (div ([class "center"]) (span ([class "small"]) ,(find-next-post post)) (br) "Next (ordered)" ,@(if (get-autoplay-cookie req) null '((br) (span ([class "autoplay"]) "autoplaying next")))))
                     (div ([class "button"] [onclick "showcomment();"])
                          (div ([class "center"])
                             ,(increment-webm-view-counter post) " views" (br)
                             "Show "
                             (a ([id "disqus_comments"] [href ,(string-append disqus-site post "#disqus_thread")]) (span ((class "loading")) "") " Comments")))
                     (a ([class "button"] [href "/list"])
                        (div ([class "center"])
                            ,(string-append (count-webms) " Gondolas") (br) "Show All/Info")))
                (div ([id "disqus_thread"] [hidden ""]))
                (script ([type "text/javascript"] [src "js/disqus.js"]))
                (script ([id "dsq-count-scr"] [src "//evo-1.disqus.com/count.js"] [async ""]))
                (noscript "Please enable JavaScript to view the " (a ([href "https://disqus.com/?ref_noscript"]) "comments powered by Disqus."))))))))

(define/timemo list-all (120 (current-directory "htdocs") reloadable-safe-thread trce*)
  (response/xexpr
    #:preamble #"<!DOCTYPE html>"
    `(html
       (head
         ,@common-header
         (title "All Gondolas - GondolaArchive"))
       (body
         (a ([href "/archive/gondolas.zip"]) "Download All (zip file)")
         (p "Public API: " (a ([href ,(blog-url redirect-random)]) ,(blog-url redirect-random)) " redirects to a random gondola. "
            (a ([href ,(blog-url redirect-random-raw)]) ,(blog-url redirect-random-raw)) " redirects to a random gondola video stream.")
         (p "N/A on the view count indicates high load, so the view count is not loaded. View count since 2017-09-17T18:42:49+0200")
         (p "Video can be looped in most browsers: right-click -> loop")
         (p "Videos normally autoplay. If you click Next (ordered) autoplay will play sequentually, if you click Next (random) autoplay will play in random order. Log: " (a ([href "/logs/log"]) "/logs/log") ", colored log (ansi color codes): " (a ([href "/logs/color-log"]) "/logs/color-log"))
         (p "Gondola suggestions: macocio@gmail.com")
         (br)
         ,@(create-list-table)))))

(define-syntax (for/fold-let stx)
  (syntax-parse stx
    [(_ ([entry value] ...) others ... finalizer)
     #'(let-values ([(entry ...)
          (for/fold ([entry value] ...)
                    others ...)])
          finalizer)]))

(define-syntax (time-ago stx)
  (syntax-parse stx
    [(_ value:expr (limit:expr singular:expr plural:expr) ... (zero:expr singular*:expr plural*:expr))
     #'(cond
          ([> (truncate (/ value limit)) 0] (let ([actual (exact-truncate (truncate (/ value limit)))])
                                              (string-append (number->string actual)
                                                             (match actual
                                                               (1 singular)
                                                               (_ plural))))) ...
          (else (match value
                       (0 zero)
                       (1 (string-append (number->string (exact-truncate value)) singular*))
                       (_ (string-append (number->string (exact-truncate value)) plural*)))))
        ]))

(define-syntax (list-let stx)
  (syntax-parse stx
    [(self variable:expr (name:id names:id ...+) code:expr ...+)
     #'(let ([name (first variable)])
         (self (rest variable) (names ...) code ...))]
    [(_ variable:expr (name:id) code:expr ...+)
     #'(let ([name (first variable)])
         code ...)]))

(define-syntax (lambda-list-let stx)
  (syntax-parse stx
    [(_ (names:id ...+) code:expr ...+)
     #'(lambda (x) (list-let x (names ...) code ...))]))

(define (compute-time-ago diff)
  (time-ago diff
            [(* 3600 24 365.25) " year ago" " years ago"]
            [(* 3600 24 30.44) " month ago" " months ago"]
            [(* 3600 24 7) " week ago" " weeks ago"]
            [(* 3600 24) " day ago" " days ago"]
            [(* 3600) " hour ago" " hours ago"]
            [(* 60) " minute ago" " minutes ago"]
            ["Just now" " second ago" " seconds ago"]))

(define (compute-source-completion webms)
  (let ([source-count (foldl (lambda (x accum) (if (string=? x "") accum (add1 accum))) 0 (map third webms))])
    (exact->inexact (* 100 (/ source-count (length webms))))))

(define (safe-file-or-directory-modify-seconds path)
  (with-handlers ([identity (lambda e 0)])
    (file-or-directory-modify-seconds path)))

(define (create-list-table)
  (let-values ([(views webms) (get-all-webm-with-views-and-source)])
    (let ([times (sort (map (lambda-list-let (filename)
                              (list filename (safe-file-or-directory-modify-seconds (build-path "video" filename))))
                            webms) > #:key second)])
      (let ([current-time (current-seconds)])
        `((p "Recently added Gondolas")
          (div ([class "small-scroll"])
            (table
              ,@(map (lambda-list-let (filename views)
                       `(tr (th ,(compute-time-ago (- current-time views)))
                            (th (a ((href ,filename)) ,filename))
                            (th ,(date->string (seconds->date views) #t))))
                     times)))
          (br)
          (p "There are " (span ([class "rainbow-block"]) ,(number->string (length webms)))
             " Gondolas in this archive. "
             (span ([class "rainbow-block"]) ,(real->decimal-string (compute-source-completion webms) 2) "%") " of Gondolas have a source. Last render: " ,(date->string (seconds->date (current-seconds)) #t))
          (br)
          (table ([class "source-table"])
            (tr (th "Gondola (by name)") (th "Views") (th "Source"))
            (tr (th "-------") (th "-----") (th "-----"))
            ,@(webm-table-alphabetical views webms))
          (table ([class "view-table"])
            (tr (th "Gondola (by views)") (th "Views"))
            (tr (th "-------") (th "-----"))
            ,@(webm-table-by-views views webms)))))))

(define (numeric-compare x y)
  (cond
    ([< x y] 'less)
    ([= x y] 'equal)
    ([> x y] 'greater)))

(define (compare-number-strings x y)
  (numeric-compare
    (with-handlers ([identity (lambda e 0)])
      (string->number x))
    (with-handlers ([identity (lambda e 0)])
      (string->number y))))

(define (compare-number-strings-then-name x y)
  (match (compare-number-strings (second x) (second y))
    ('less #t)
    ('equal (string>=? (first x) (second x)))
    ('greater #f)))

(define tabulate-webm
  (lambda-list-let (filename views)
    `(tr (th (a ([href ,filename]) ,filename)) (th ,views))))

(define tabulate-webm-source
  (lambda-list-let (filename views source)
    `(tr (th (a ([href ,filename]) ,filename)) (th ,views) (th ,source))))

(define (webm-table-by-views views webms)
  (webm-table views webms compare-number-strings-then-name))

(define (webm-table-alphabetical views webms)
  (let ([sorted (sort webms string<=? #:key first)])
    (cons `(tr (th "Total") (th ,(number->string views)) (th ""))
      (cons '(tr (th "-------") (th "-----") (th "-----"))
        (map tabulate-webm-source sorted)))))

(define (get-all-webm-with-views-and-source)
  (let-values ([(sum-views webms) (get-all-webm-with-views)])
    (values
      sum-views
      (map (lambda-list-let (webm views)
             (list webm views (post-source webm)))
           webms))))

(define (get-all-webm-with-views)
  (for/fold-let ([sum-views 0]
                 [table empty])
                ([x (get-all-webm)])
    (let ([views (get-webm-view-count x)])
      (values
        (with-handlers ([identity (lambda (e) sum-views)])
          (+ (string->number views) sum-views))
        (cons (list x views) table)))
    (values sum-views table)))

(define (webm-table views webms sorter #:key [key identity])
  (let ([sorted (sort webms sorter #:key key)])
    (cons `(tr (th "Total") (th ,(number->string views)))
      (cons '(tr (th "-------") (th "-----"))
        (map tabulate-webm sorted)))))

(define (redirect-random req)
  (redirect-to (get-random-page) #:headers (list (cookie->header play-random)) temporarily))

(define (redirect-random-raw req)
  (redirect-to (get-random-page-raw) temporarily))

(define-values (blog-dispatch blog-url)
  (dispatch-rules
    (("random") redirect-random)
    (("random-raw") redirect-random-raw)
    (("robots.txt") (lambda _ (redirect-to "/misc/robots.txt" permanently)))
    (("next") serve-next)
    (("list") list-all)
    (("favicon.ico") (lambda _ (redirect-to "/images/musings_symbol_128.png" permanently)))
    (((string-arg)) serve-post)
    (else (lambda _ (redirect-to default-video)))))

(define (file-not-found req)
  (redirect-to default-video))
