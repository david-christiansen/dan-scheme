#lang racket/base

(require (for-syntax racket/base syntax/parse))

(provide cons car cdr pair? null?
         quote quasiquote unquote
         equal? eqv?
         + - add1 sub1 * / number?
         = > < >= <=
         memv assv
         map list-ref
         apply
         and or not
         cond if
         let
         syntax-rules (for-syntax ...)
         require
         provide all-defined-out for-syntax
         #%module-begin
         #%datum
         #%top-interaction
         (rename-out [dan-lambda lambda]
                     [dan-define define]
                     [dan-define-syntax define-syntax]
                     [#%plain-app #%app]))

(define-for-syntax (sum nums)
  (cond
    [(null? nums) 0]
    [(pair? nums) (+ (car nums) (sum (cdr nums)))]))

(define-for-syntax (ensure-helper arg-id body result)
  (syntax-parse body
    [(f arg ...)
     (let ([arg-use-count
            (sum (map (lambda (x)
                        (if (and (identifier? x)
                                 (free-identifier=? x arg-id))
                            1
                            0))
                      (syntax->list #'(arg ...))))])
        (cond
          [(= arg-use-count 1)
           result]
          [(> arg-use-count 1)
           (raise-syntax-error
            'lambda
            "uses of (lambda args ...) should provide args to the helper just once")]
          [else
           (raise-syntax-error
            'lambda
            "uses of (lambda args ...) must pass args directly to a helper")]))]
    [x:id
     #:when (free-identifier=? arg-id #'x)
     result]
    [_ (raise-syntax-error 'lambda "uses of (lambda args) must pass args to a helper")]))

(define-syntax (dan-lambda stx)
  (syntax-parse stx
    [(_ (x:id ...) body)
     #'(#%plain-lambda (x ...) body)]
    [(_ args:id body)
     (ensure-helper #'args #'body #'(#%plain-lambda args body))]))

(define-syntax (dan-define stx)
  (syntax-parse stx
    [(_ x:id e:expr)
     #'(define x e)]
    [(_ (f:id x:id ...) e:expr)
     #'(define f (dan-lambda (x ...) e))]
    [(_ (f:id x:id ... . args) e:expr)
     (raise-syntax-error 'define "define doesn't get to use the (f x . args) form, use lambda")]))

(define-syntax (dan-define-syntax stx)
  (syntax-parse stx
    #:literals (syntax-rules)
    [(_ x:id (syntax-rules (lit ...) pat ...))
     #'(define-syntax x (syntax-rules (lit ...) pat ...))]
    [_ (raise-syntax-error 'define-syntax "only syntax-rules macros exist in dan-scheme")]))

(module reader syntax/module-reader
  dan-scheme
  #:wrapper1 (lambda (r)
               (parameterize ([read-case-sensitive #t]
                              [read-accept-box #f]
                              [read-accept-compiled #f]
                              [read-accept-bar-quote #t]
                              [read-accept-graph #f]
                              [read-accept-dot #t]
                              [read-accept-infix-dot #f]
                              [read-cdot #f]
                              [read-accept-quasiquote #t]
                              [read-accept-reader #f])
                 (r))))
