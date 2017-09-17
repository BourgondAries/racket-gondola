#lang racket

(provide blog-dispatch file-not-found)

(require (for-syntax racket/list)
         (for-syntax racket/pretty)
         (for-syntax racket/syntax)
         (for-syntax syntax/parse)
         web-server/dispatch
         web-server/servlet
         web-server/servlet-env)

(require "settings.rkt")

(define (serve-index req)
  (serve-post req default-video))

(define (select-random lst)
  (let ([len (length lst)])
    (list-ref lst (random len))))

(define (get-all-webm)
  (map path->string (directory-list "video")))

(define (increment-webm-view-counter webm)
  (let ([target (build-path "statistics" webm)])
    (call-with-file-lock/timeout target 'exclusive
      (lambda ()
        (if (file-exists? target)
          (with-input-from-file target
            (lambda ()
              (let ([increment (add1 (string->number (read-line)))])
                (with-output-to-file target
                  (lambda ()
                    (write increment)
                    (number->string increment)) #:exists 'truncate))))
          (with-output-to-file target
            (lambda () (write 1)
              "1")
            #:exists 'truncate)))
      (lambda ()
        (displayln "N/A")))))

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
                                   "N/A"))))

(define (count-webms)
  (number->string (length (get-all-webm))))

(define (get-random-page)
  (get-random-webm))

(define (get-random-page-raw)
  (string-append "/video/" (get-random-webm)))

(define (serve-post req post)
  (if (not (file-exists? (string-append "video/" post)))
    (redirect-to (get-random-page))
    (response/xexpr
      #:preamble #"<!DOCTYPE html>"
      `(html
        (head
          (meta ([charset "UTF-8"]))
          (meta ([name "viewport"] [content "width=device-width,maximum-scale=1,minimum-scale=1"]))
          (link ([rel "icon"] [type "image/png"] [href "/images/musings_symbol_16.png"]))
          (link ([rel "icon"] [type "image/png"] [href "/images/musings_symbol_32.png"]))
          (link ([rel "icon"] [type "image/png"] [href "/images/musings_symbol_64.png"]))
          (link ([rel "stylesheet"] [type "text/css"] [href "/css/reset.css"]))
          (style
            "button { background: rgba(32, 40, 45, 0.3); border: 1px solid #1C252B; color: lightgrey; font-size: 1em; height: 100%; left: 0%; position: relative; transform: translate(0%, 0); vertical-align: top; white-space: normal; width: 33.2%; }
            button:hover { cursor: pointer; }
            .blog { background-image: url(\"/images/sharding.jpg\"); background-size: 100%; background-repeat: y; position: relative; }
            .video { height: 88vh; }
            .bottom { color: white; font-family: arial; height: 10vh; margin-bottom: 1vh; margin-top: 1vh; margin-left: 1vw; margin-right: 1vw; }
            #disqus_thread { background: rgba(64, 80, 90, 0.2); padding: 0 1vw 0 1vw; }")
          (title "GondolaArchive")
          (body ([class "blog"])
                (div ([class "video"])
                     (video ([id "video"] [width "100%"] [height "100%"] [onclick "toggle_pause();"] [autoplay ""] [controls ""])
                            (source ((src ,(string-append "/video/" post)) (type "video/webm")))))
                (script ([type "text/javascript"])
                        "document.getElementById('video').addEventListener('ended', ended, false);
                        function ended(handle) {
                            history.pushState({
                              prevUrl: window.location.href
                            }, 'Next page', \"/random\");
                            history.go();
                        }
                        function toggle_pause() {
                            if (document.getElementById('video').paused) {
                               document.getElementById('video').play();
                            } else {
                               document.getElementById('video').pause();
                            }
                        }")
                (script ([type "text/javascript"])
                        "function showcomment(handle) {
                            document.getElementById('disqus_thread').style.display = 'block';
                        }
                        ")
                (div ([class "bottom"])
                     (button ([type "button"] [onclick "ended();"]) "Next (random)")
                     (button ([type "button"] [onclick "showcomment();"]) "Views: " ,(increment-webm-view-counter post) (br) "Show comments")
                     (button ([type "button"] [onclick "location.href='/list';"])
                            ,(string-append "All" " " "(" (count-webms) ")")))
                (div ([id "disqus_thread"] [hidden ""]))
                (script
                  "/**
                  *  RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
                  *  LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables*/
                  /*
                  var disqus_config = function () {
                    this.page.url = \"gondola\";  // Replace PAGE_URL with your page's canonical URL variable
                    this.page.identifier = \"" ,post \""; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
                  };
                  */
                  (function() { // DON'T EDIT BELOW THIS LINE
                    var d = document, s = d.createElement('script');
                    s.src = '//evo-1.disqus.com/embed.js';
                    s.setAttribute('data-timestamp', +new Date());
                    (d.head || d.body).appendChild(s);
                  })();")
                (noscript "Please enable JavaScript to view the " (a ([href "https://disqus.com/?ref_noscript"]) "comments powered by Disqus."))))))))

(define (list-all req)
  (response/xexpr
    #:preamble #"<!DOCTYPE html>"
    `(html
       (head
         (meta ([charset "UTF-8"]))
         (meta ([name "viewport"] [content "width=device-width,maximum-scale=1,minimum-scale=1"]))
         (link ([rel "icon"] [type "image/png"] [href "/images/musings_symbol_16.png"]))
         (link ([rel "icon"] [type "image/png"] [href "/images/musings_symbol_32.png"]))
         (link ([rel "icon"] [type "image/png"] [href "/images/musings_symbol_64.png"]))
         (link ([rel "stylesheet"] [type "text/css"] [href "/css/reset.css"])))
         (style
           "tr:nth-child(even) {
             background-color: #EEEEEE;
           }")
       (body
         (a ([href "/archive/gondolas.zip"]) "Download All (zip file)")
         (p "Public API: " (a ([href "/random"]) "/random") " redirects to a random gondola. " (a ([href "/random-raw"]) "/random-raw") " redirects to a random gondola video stream.")
         (p "N/A on the view count indicates high load, so the view count is not loaded.")
         (p "Gondola suggestions: macocio@gmail.com")
         (br)
         (table
           (tr (th "Gondola") (th "Views"))
           (tr (th "-------") (th "-----"))
           ,@(link-all-webm))
         )
  )))

(define-syntax (for/fold-let stx)
  (syntax-parse stx
    [(_ ([entry value] ...) others ... finalizer)
     #'(let-values ([(entry ...)
          (for/fold ([entry value] ...)
                    others ...)])
          finalizer)]))

(define (link-all-webm)
  (let-values ([(sum-views table)
    (for/fold ([sum-views 0]
               [table empty])
              ([x (get-all-webm)])
        (let ([views (get-webm-view-count x)])
          (values
            (with-handlers ([identity (lambda (e) sum-views)])
              (+ (string->number views) sum-views))
            (cons `(tr (th (p (a ([href ,x]) ,x)) (th ,views))) table)
      )))])
      (cons `(tr (th "Total") (th ,(number->string sum-views))) table)
    ))

(define-values (blog-dispatch blog-url)
  (dispatch-rules
    (("random") (lambda _ (redirect-to (get-random-page))))
    (("random-raw") (lambda _ (redirect-to (get-random-page-raw))))
    (("list") list-all)
    (((string-arg)) serve-post)
    (else (lambda _ (redirect-to default-video)))))

(define (file-not-found req)
  (redirect-to default-video))
