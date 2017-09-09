#lang racket

(provide blog-dispatch file-not-found)

(require (for-syntax racket/list)
         (for-syntax racket/pretty)
         web-server/dispatch
         web-server/servlet
         web-server/servlet-env)

(require "settings.rkt")

(define (serve-index req)
  (serve-post req default-video))

(define (get-all-webm)
  (with-output-to-string (lambda ()
                           (system/exit-code "/usr/bin/env bash -c 'find music -maxdepth 1 -name \"*.webm\" -type f | sed s/^music//' | sort"))))
(define (get-random-webm-raw)
  (with-output-to-string (lambda ()
                           (system/exit-code "/usr/bin/env bash -c 'find music -maxdepth 1 -name \"*.webm\" -type f | shuf | head -n 1'"))))
(define (get-random-webm)
  (with-output-to-string (lambda ()
                           (system/exit-code "/usr/bin/env bash -c 'find music -maxdepth 1 -name \"*.webm\" -type f | shuf | head -n 1 | sed s/^music//'"))))

(define (count-webms)
  (with-output-to-string (lambda ()
                           (system/exit-code "/usr/bin/env bash -c 'find music -maxdepth 1 -name \"*.webm\" -type f | wc -l | xargs echo -n'"))))

(define (get-random-page)
  (get-random-webm))

(define (get-random-page-raw)
  (get-random-webm-raw))

(define (serve-post req post)
  (if (not (file-exists? (string-append "music/" post)))
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
            "button { background: rgba(32, 40, 45, 0.3); border: 1px solid #1C252B; color: lightgrey; font-size: 1em; height: 100%; left: 0%; position: relative; transform: translate(0%, 0); width: 33.333%; }
            button:hover { cursor: pointer; }
            .blog { background-image: url(\"/images/sharding.jpg\"); background-size: 100%; background-repeat: y; position: relative; }
            .video { height: 88vh; }
            .bottom { color: white; font-family: arial; height: 10vh; margin-bottom: 1vh; margin-top: 1vh; margin-left: 1vw; margin-right: 1vw; }
            #disqus_thread { background: rgba(64, 80, 90, 0.2); padding: 0 1vw 0 1vw; }")
          (title "GondolaArchive")
          (body ([class "blog"])
                (div ([class "video"])
                     (video ([id "video"] [width "100%"] [height "100%"] [onclick "toggle_pause();"] [autoplay ""] [controls ""])
                            (source ((src ,(string-append "/music/" post)) (type "video/webm")))))
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
                     (button ([type "button"] [onclick "showcomment();"]) "Show comments")
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
       (body
         (a ((href "/music/gondolas.zip")) "Download All (zip file)")
         (br)
         (br)
         ,@(link-all-webm))
  )))

(define (link-all-webm)
  (map
    (lambda (x)
      `(p (a ([href ,x]) ,x)))
    (string-split (get-all-webm) "\n")))


(define-values (blog-dispatch blog-url)
  (dispatch-rules
    (("random") (lambda _ (redirect-to (get-random-page))))
    (("random-raw") (lambda _ (redirect-to (get-random-page-raw))))
    (("list") list-all)
    (((string-arg)) serve-post)
    (else (lambda _ (redirect-to default-video)))))

(define (file-not-found req)
  (redirect-to default-video))
