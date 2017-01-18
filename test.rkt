#lang dan-scheme
(require (prefix-in r: racket)
         (only-in syntax/location quote-srcloc)
         (only-in setup/path-to-relative path->relative-string/library)
         (for-syntax
          (only-in racket
                   quasisyntax unsyntax
                   #%app #%datum #%top
                   quote syntax)
          (prefix-in r: racket/base)))
(require rackunit)

(r:define-syntax (convert-syntax-error stx)
  (r:syntax-case stx ()
    [(_ expr)
     (r:with-handlers ([r:exn:fail:syntax?
                      (r:lambda (e)
                        #`(r:error '#,(r:exn-message e)))])
       (r:parameterize ((r:error-print-source-location #f))
         (r:local-expand #'expr 'expression r:null)))]))

(define (f x arg)
  (+ (add1 x) 3))

(check-equal? (f 3 'whatever) 7)

(define foo
  (lambda args (cons 1 args)))

(check-equal? (map (λ (x) (add1 x)) '(1 2 3 4))
              '(2 3 4 5))

(check-equal? (map foo '(a b c))
              '((1 a) (1 b) (1 c)))

(check-exn r:exn:fail?
           (λ ()
             (convert-syntax-error
              (lambda args (if (pair? args) #t #f)))))

(check-exn r:exn:fail?
           (λ ()
             (convert-syntax-error
              (lambda args (append args args)))))

(check-exn r:exn:fail?
           (λ ()
             (convert-syntax-error
              (define (f . args) args))))

(check-exn r:exn:fail?
           (λ ()
             (convert-syntax-error
              (define-syntax (thing stx)
                (r:syntax-case stx ()
                  ((_ x) #'x))))))

;; -----------------------------------------------------------------------------

;; (check-exn/loc expr)
;; Tests that evaluating `expr` raises an exception
;;  and that the exception references a function defined on the same line
;;  as the `(check-exn/loc expr)` expression appears in this file.
(r:define-syntax (check-exn/loc stx)
  (r:syntax-case stx ()
    [(_ expr)
     (r:quasisyntax/loc stx
       (check-exn
        (r:lambda (e)
          (r:and (r:exn:fail? e)
            (r:let* ([ctx (r:continuation-mark-set->context (r:exn-continuation-marks e))]
                     [srcloc (r:cdar ctx)]
                     [expected-srcloc  (quote-srcloc expr)])
              (r:and
               (r:string=? (path->relative-string/library (r:srcloc-source srcloc))
                           (path->relative-string/library (r:srcloc-source expected-srcloc)))
               (r:printf "lines ~a ans ~a~n" (r:srcloc-line srcloc)
                    (r:srcloc-line expected-srcloc))))))
        #,(r:quasisyntax/loc stx (lambda () #,(r:syntax/loc stx expr)))
        "make sure the argument to `check-exn/loc` does not span multiple lines"))]))

(check-exn/loc (map (lambda (x) (+ x x)) '(1 two 3)))

(check-exn/loc (r:begin (define (f x) (car x)) (f 3)))

(check-exn/loc (r:begin (define f (lambda (x) (add1 x))) (f 'hello)))
