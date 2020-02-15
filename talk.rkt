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

(define (demo)
  (with-font "Georgia"
    (slide
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

(demo)

(let ([it (lambda (s)
            (para (small (t s))))])
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

(slide
 #:title "Form validation"
 (small (para (lib formlets) (t "come with") (lib web-server-lib) (t "but...")))
 (item (small (t "they couple presentation to validation")))
 (item (small (t "no easy way to include validation errors into the output")) ))

(let* ([step-c 0]
       [step (lambda (s)
               (set! step-c (add1 step-c))
               (small (red (t (format "~a. ~a" step-c s)))))])
  (slide
   #:title (para (titlet "Form validation:") (lib forms-lib) #:align 'center)
   #:name "forms-lib"
   (ht-append
    20
    (vl-append
     10
     (step "Declare a form...")
     (small
      (small (code
              (define login-form
                (form* ([username (ensure binding/email (required))]
                        [password (ensure binding/text (required))])
                  (list username password)))))))
    (vl-append
     10
     (step "Declare a renderer for it...")
     (small
      (small (code
              (define (render-login-form rw)
                `(form
                  (label "Username" ,(rw "username" (widget-email)))
                  ,@(rw "username" (widget-errors))
                  (label "Passowrd" ,(rw "password" (widget-password)))
                  ,@(rw "password" (widget-errors))
                  (button ([type "submit"]) "Login"))))))))
   (vl-append
    10
    (step "Validate requests against it...")
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
                  (redirect-to "/dashboard")]))))))))

(demo)


;; Database migrations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(slide
 #:title (para (titlet "Database migrations:") (lib north) #:align 'center)
 #:name "north"
 (ht-append
  20
  (vl-append
   10
   (small (red (t "A language for declaring migrations:")))
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
                     ))))
  (vl-append
   10
   (small (red (t "A CLI tool for managing them:")))
   (small
    (vl-append
     10
     (code |raco north create|)
     (code |raco north migrate|)
     (code |raco north rollback|)
     (code |raco north show|))))))

(demo)


;; ORM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(slide
 #:title (para (titlet "ORM:") (lib deta-lib) #:align 'center)
 #:name "deta"
 (subscript (t "* not really an ORM")))

(demo)


;; E2E Testing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(slide
 #:title (para (titlet "E2E testing:") (lib marionette) #:align 'center)
 #:name "marionette")

(demo)


;; Koyo ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(slide
 #:title (para (titlet "Everything else:") (lib koyo-lib) #:align 'center)
 #:name "koyo")

(demo)


;; Fin ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-font "Georgia"
  (slide
   (caps (big (t "Thanks!")))
   (red (t "defn.io"))))
