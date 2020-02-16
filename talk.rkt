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

(define-syntax-rule (demo name e ...)
  (with-font "Georgia"
    (slide
     #:name (format "demo ~a" name)
     (caps (big (t "Demo")))
     e ...)))

(define (comment s)
  (define p
    (with-font "Gill Sans"
      (colorize
       (small (small (small (small (apply para (map t (string-split s)))))))
       (hexcolor "948c94"))))
  (define bg
    (colorize
     (filled-rectangle (+ (pict-width p) 20)
                       (+ (pict-height p) 20))
     (hexcolor "f2f2f2")))
  (cc-superimpose bg p))

(with-font "Georgia"
  (slide
   (caps (big (t "Stepping up Racket")))
   (caps (big (t "for the Web")))
   (small (t "Bogdan Popa"))

   (comment #<<COMMENT
Hello everybody!  My name is Bogdan and today I'm going to talk about
Stepping up Racket for the Web.
COMMENT
            )))

(let ()
  (define-syntax-rule (it name description)
    (para (small (para (code name) (t description)))))

  (slide
   #:title "Batteries"
   (it racket "a powerful programming environment")
   (comment #<<COMMENT
Racket comes with an extensive set of batteries in its standard
distribution.  First, we've got Racket itself, which is a powerful
programming environment.
COMMENT
            )

   'next
   (it web-server-lib "a capable Web Server built for high concurrency")
   (comment #<<COMMENT
Next is web-server-lib, which is a capable web server built for high
concurrency and with support for some pretty interesting features such
as continuations.
COMMENT
            )

   'next
   (it db-lib "a DB library that supports all the major OSS RDBMS")
   (comment #<<COMMENT
Then there's db-lib, which has support for all the major RDBMS,
including MySQL, PostgreSQL, SQLite3 and OracleDB.
COMMENT
            )

   'next
   (it rackunit-lib "a standardized way to test code")
   (comment #<<COMMENT
A standardized way to test code across the ecosystem with rackunit-lib.
COMMENT
            )

   'next
   (it |raco exe| "an easy way to package and distribute code")
   (comment #<<COMMENT
And raco exe, which provides a convenient way to package together an
entire application and its associated resources and then distribute it
to a server that doesn't even have Racket installed.
COMMENT
            )))

(demo "matchacha"
      (comment #<<COMMENT
My girlfriend wanted to start selling green tea online so last year I
built an e-commerce website to support that using Racket.  Here it
is...
COMMENT
               ))

(let ([it (lambda (s)
            (para (t s)))])
  (slide
   #:title "What did I build?"
   (comment #<<COMMENT
So what did I have to build on top of the standard distribution to get
all that working?  Well, a few things...
COMMENT
            )

   'next
   (it "form validation*")
   (comment #<<COMMENT
First, there's form validation.  There is some support for this in the
web-server package, but I'll talk about why that's not enough in a
minute.
COMMENT
            )

   'next
   (it "database migrations")
   (comment #<<COMMENT
Then there's database migrations.  How do you manage schema versioning
from one release to another?
COMMENT
            )

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
 (vl-append
  10
  (item (t "they couple presentation to validation"))
  (item (t "no easy way to include validation errors into the output"))
  (comment #<<COMMENT
First, let's start with form validation.  As I mentioned, there is
support for form validation in the web-server package via formlets.
Unfortunately, formlets have a couple of problems: because they're
declared in line with xexpressions, they are coupled to the
presentation and there's no easy way to extract and present validation
errors to the user.
COMMENT
           )))

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
                   (redirect-to "/dashboard")])))))))

   (comment #<<COMMENT
That leads me to the first library that I built, which is forms-lib.
It lets you declaratively specify how a form ought to be validated and
what the result of that validation should be.  In the top left, I'm
declaring a form called login-form and I'm specifying that it must
contain a username field that's an e-mail address and is required and
password field that is arbitrary text that is also required.  The
result of a successful form validation is a list containing the
username followed by the password.

Separate from this declaration is a function that knows how to render
such a form.  I'm not going to go into the details here, but the
mechanics of how forms work make it very easy for these renderers to
display previous form values as well as errors to the user.

Finally, you use a form by running it against a request.  This
produces three possible branches:

1. either the form is in a pending state, meaning that it hasn't been
submitted yet.  i.e. the user just landed on the page.  In that case
we render the empty form for them.

2. once they submit the form, it can be either failed or passed.  If
it's failed, we do the same thing and render it.  The renderer that we
previously defined will be able to display any errors to the user.

3. finally, once they fix all the errors, the last state is the passed
state in which we get back the list containing the valid username and
password, log the user in and send them on their way.
COMMENT
            )))

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
   (vc-append
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
