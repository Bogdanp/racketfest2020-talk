#lang slideshow

(require slideshow/code
         slideshow/text)

(set-margin! 40)
(current-main-font "Gill Sans")
(current-code-font "Dank Mono")
(current-titlet
 (lambda (s)
   (colorize (text s "Georgia" 36)
             (current-title-color))))

(define (hexcolor s)
  (for/list ([i (in-range 3)])
    (define lo (* i 2))
    (define hi (+ lo 2))
    (string->number (substring s lo hi) 16)))

(current-slide-assembler
 (let ([old (current-slide-assembler)])
   (lambda (t sep p)
     (define bg
       (inset (colorize (filled-rectangle 1024 768) (hexcolor "f6f6f6"))
              (- margin)))
     (refocus (ct-superimpose bg (old t sep p)) bg))))

(define-syntax-rule (define-color id s)
  (define-syntax-rule (id e)
    (colorize e (hexcolor s))))

(define-color red "bf0006")

(define-syntax-rule (lib s)
  (red (code s)))

(define (demo name)
  (with-font "Georgia"
    (slide
     #:name (format "demo ~a" name)
     (caps (big (t "Demo"))))))

(with-font "Georgia"
  (slide
   (caps (big (t "Stepping up Racket")))
   (caps (big (t "for the Web")))
   (small (t "Bogdan Popa"))))

(let ()
  (define-syntax-rule (it name description)
    (para (small (para (code name) (t description)))))

  (slide
   #:title "Batteries"
   (it racket "a powerful programming environment")
   'next
   (it web-server-lib "a capable Web Server built for high concurrency")
   'next
   (it db-lib "a DB library that supports all the major OSS RDBMS")
   'next
   (it rackunit-lib "a standardized way to test code")
   'next
   (it |raco exe| "an easy way to package and distribute code")))

(demo "matchacha")

(let ([it (lambda (s)
            (para (t s)))])
  (slide
   #:title "What's missing?"
   (it "form validation*")
   'next
   (it "database migrations")
   'next
   (it "an ORM*")
   'next
   (it "end-to-end testing")
   'next
   (it "session management")
   'next
   (it "profiling")))


;; Form validation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (roman n)
  (cond
    [(< n 4) (make-string n #\i)]
    [(= n 4) "iv"]
    [(< n 9) (~a "v" (make-string (- n 5) #\i))]
    [(= n 9) "ix"]
    [(> n 9) (~a "x" (roman (- n 10)))]))

(define (pill p)
  (define bg (colorize
              (filled-rounded-rectangle (+ (pict-width p) 20)
                                        (+ (pict-height p) 10)
                                        10)
              "white"))
  (cc-superimpose bg p))

(define (make-stepper)
  (define count 0)
  (lambda (s)
    (set! count (add1 count))
    (define txt (small (red (t (format "~a. ~a" (roman count) s)))))
    (define wrap (colorize
                  (filled-rectangle (pict-width txt)
                                    (+ (pict-height txt) 20)
                                    #:border-width #f)
                  "white"))
    (pill (lt-superimpose wrap txt))))

(slide
 #:title "Form validation"
 (para (lib formlets) (t "come with") (lib web-server-lib) (t "but..."))
 (item (t "they couple presentation to validation"))
 (item (t "no easy way to include validation errors into the output")))

(let ([step (make-stepper)])
  (slide
   #:title (para (titlet "Form validation:") (lib forms-lib) #:align 'center)
   #:name "forms-lib"
   (ht-append
    20
    (vl-append
     -20
     (step "Declare a form...")
     (pill
      (small
       (small (code
               (define login-form
                 (form* ([username (ensure binding/email (required))]
                         [password (ensure binding/text (required))])
                   (list username password))))))))
    (vl-append
     -20
     (step "Declare a renderer for it...")
     (pill
      (small
       (small (code
               (define (render-login-form rw)
                 `(form
                   (label "Username" ,(rw "username" (widget-email)))
                   ,@(rw "username" (widget-errors))
                   (label "Passowrd" ,(rw "password" (widget-password)))
                   ,@(rw "password" (widget-errors))
                   (button ([type "submit"]) "Login")))))))))
   (vl-append
    -20
    (step "Validate requests against it...")
    (pill
     (small
      (small (code
              (define (login req)
                (match (form-run login-form req)
                  [(list 'pending _ rw)
                   (response/xexpr
                    (render-login-form rw))]
                  (code:line)
                  [(list 'failed errors rw)
                   (response/xexpr
                    (render-login-form rw))]
                  (code:line)
                  [(list 'passed (list username password) rw)
                   (login! username password)
                   (redirect-to "/dashboard")])))))))))

(demo "forms-lib")


;; Database migrations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ([step (make-stepper)])
  (slide
   #:title (para (titlet "Database migrations:") (lib north) #:align 'center)
   #:name "north"
   (ht-append
    -50
    (vl-append
     -20
     (step "A #lang for declaring migrations...")
     (pill
      (small
       (small
        (codeblock-pict #<<CODE
#lang north

-- @revision: 2b39dc065de3cd5ebb612e2d80f00d8c
-- @description: Creates the "users" table.
-- @up {
create table users(
  id serial primary key,
  first_name text not null,
  last_name text not null,
  username text not null unique,
  password_hash text not null,
  verified boolean not null default false,
  verification_code text not null,
  created_at timestamptz not null default current_timestamp,
  updated_at timestamptz not null default current_timestamp,

  constraint users_username_is_lowercase check(username = lower(username))
);
-- }

-- @down {
drop table users;
-- }
CODE
                        )))))
    (vl-append
     -20
     (step "A CLI tool for managing them...")
     (pill
      (small
       (codeblock-pict #<<CODE
#lang rash
raco north create
raco north migrate
raco north rollback
raco north show
CODE
                       #:keep-lang-line? #f
                       )))))))

(demo "north")


;; ORM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ([step (make-stepper)])
  (slide
   #:title (para (titlet "ORM:") (lib deta-lib) #:align 'center)
   #:name "deta"
   (t "An ORM without the \"R\"elational part.")
   (vl-append
    20
    (vl-append
     -20
     (step "Maps table rows to structs...")
     (pill
      (vl-append
       10
       (small
        (code
         (define-schema user
           #:table "users"
           ([id id/f #:primary-key #:auto-increment]
            [username string/f]
            [(password-hash #f) string/f]))))

       (small
        (code
         (define u
           (make-user #:username "Bogdan"))))

       (small
        (code
         (insert! conn u))))))
    (vl-append
     -20
     (step "Constructs SQL queries using Racket DSL...")
     (pill
      (small
       (code
        (lookup conn (~> (from user #:as u)
                         (where (= u.username "Bogdan")))))))))))

(slide
 #:title (para (titlet "ORM:") (lib deta-lib) (t "cont'd.") #:align 'center)
 #:name "deta 2"
 (t "Handles arbitrary result sets. Great for analytical queries:")
 (ht-append
  10
  (pill
   (small
    (small
     (code
      (define-schema orders/status/month
        #:virtual
        ([status symbol/f]
         [month datetime-tz/f]
         [orders integer/f]
         [revenue integer/f]))))))

  (pill
   (small
    (small
     (code
      (~> (from "orders" #:as o)
          (join "order_statuses" #:as os #:on (= o.id os.order-id))
          (select (as os.status status)
                  (as (date-trunc "month" o.created-at) month)
                  (as (count *) orders)
                  (as 0 revenue))
          (group-by 1 2)
          (union (~> (from "orders" #:as o)
                     (join "order_totals" #:as ot #:on (= o.id ot.order-id))
                     (join "order_statuses" #:as os #:on (= o.id os.order-id))
                     (select (as "revenue" status)
                             (as (date-trunc "month" o.created-at) month)
                             (as 0 orders)
                             (as (sum ot.total) revenue))
                     (where (not (in os.status (list "canceled" "returned"))))
                     (group-by 1 2)))
          (order-by ([month #:asc]))
          (project-onto orders/status/month-schema))))))))

(demo "deta-lib")


;; E2E Testing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(slide
 #:title (para (titlet "E2E testing:") (lib marionette-lib) #:align 'center)
 #:name "marionette"
 (t "A library that lets you drive a Firefox process from Racket:")
 (pill
  (code
   (call-with-marionette/browser/page!
    (lambda (p)
      (page-goto! p "https://github.com")
      (call-with-page-screenshot! p
        (lambda (data)
          ...)))))))

(demo "marionette-lib")


;; koyo ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(slide
 #:title (para (titlet "Everything else:") (lib koyo-lib) #:align 'center)
 #:name "koyo"
 (t "A set of functionality commonly needed by web applications:")
 (item (t "Application lifecycle"))
 (item (t "CORS"))
 (item (t "Configuration management"))
 (item (t "CSRF tokens"))
 (item (t "Flash messages"))
 (item (t "Session management"))
 (item (t "Localization"))
 (item (t "Profiling & more...")))


;; Sessions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(slide
 #:title (para (titlet "Session management:") (lib koyo-lib) #:align 'center)
 #:name "koyo sessions"
 (para (lib web-server-lib) (t "has") (code make-id-cookie) (t "..."))
 (item (t "somewhat low level"))
 (item (t "storing too much data in cookies slows things down")))

(slide
 #:title (para (titlet "Session management:") (lib koyo-lib) (t "cont'd.") #:align 'center)
 #:name "koyo sessions 2"
 (para (code koyo/session) (t "high level store for session data..."))
 (vl-append
  (item (para (t "stores unique session id using") (code make-id-cookie)))
  (item (para (t "session ids map to storage backends (in-memory, Redis, etc.)")))))

(demo "koyo sessions")


;; Profiling ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ([step (make-stepper)])
  (slide
   #:title (para (titlet "Profiling:") (lib koyo-lib) #:align 'center)
   #:name "koyo profiling"
   (vl-append
    20
    (para (small (code koyo/profiler))
          (small (t "an instrumenting profiler for web applications...")))
    (vl-append
     -20
     (step "Annotate various bits of code...")
     (pill
      (small
       (code
        (define (some-fun n)
          (with-timing (format "(some-fun ~a)" n)
            (do-a-slow-thing n)))))))
    (vl-append
     -20
     (step "See traces with timing information on every request...")
     (pill
      (scale-to-fit
       (bitmap "example-profiler.png")
       400 400))))))

(demo "koyo profiling")


;; Fin ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fin)
  (with-font "Georgia"
    (slide
     #:name "fin"
     (caps (big (t "Thanks!")))
     (red (t "defn.io")))))

(fin)


;; ITT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(slide
 #:title (para (titlet "If there's time...") #:align 'center)
 #:name "itt"
 (small (para (code net-ip-lib) (t "utilities for working with IP networks and addrs")))
 (small (para (code geoip-lib) (t "IP geolocation based on MaxMind's databases")))
 (small (para (code redis-rkt) (t "fast, idiomatic Redis client library")))
 (small (para (code sentry-lib) (t "production error monitoring"))))


;; Fin (for real!) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fin)
