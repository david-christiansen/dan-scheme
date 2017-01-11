#lang dan-scheme
(require (prefix-in r: racket)
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
