#lang scribble/manual

@require[@for-label[dan-scheme @only-in[racket any/c or/c require provide all-defined-out for-syntax]]]

@title{Dan Scheme}
@author{David Christiansen}

@defmodule[dan-scheme]

This is a little Scheme-like language for situations in which
simplicity is preferred over convenience.

@local-table-of-contents[]

@section[#:tag "pairs"]{Pairs}
@defproc[(cons (a any/c) (d any/c)) pair?]{
Creates a pair whose @racket[car] is @racket[a] and whose @racket[cdr] is @racket[d].
}

@defproc[(car (p pair?)) any/c]{
Finds the first element of @racket[p].
}

@defproc[(cdr (p pair?)) any/c]{
Finds the second element of @racket[p].
}

@defproc[(pair? (p any/c)) any/c]{
Returns @racket[#t] if @racket[p] is a pair of the sort built with @racket[cons], or @racket[#f] otherwise.
}


@section[#:tag "quotations"]{Quotations}

@defform[(quote e)]{
Suspends evaluation of @racket[e], constructing an S-expression data structure. Often written with an apostrophe.
}

@defform[(quasiquote e)]{
Suspends evaluation of @racket[e], allowing a return to evaluation using @racket[unquote]. Often written with a back-quote.
}

@defform[(unquote e)]{
Resumes evaluation of @racket[e] within a @racket[quasiquote]. Often written with a comma.
}


@section[#:tag "equality"]{Equality Predicates}

@defproc[(equal? (a any/c) (b any/c)) boolean?]{
Recursively compares @racket[a] and @racket[b] for equality.
Return @racket[#f] if a difference is found, or @racket[#t] if not.
}

@defproc[(eqv? (x any/c) (y any/c)) boolean?]{
Non-recursively compares @racket[x] and @racket[y] for equality.
Return @racket[#f] if a difference is found, or @racket[#t] if not.
This should be used to compare symbols.
}


@section[#:tag "arithmetic"]{Arithmetic}

@defproc[(number? (v any/c)) boolean?]{
Checks whether @racket[v] is a number.
}

@defproc[(+ (x number?) ...) number?]{
 Computes the sum of @racket[x ...]. If no numbers are provided, returns @racket[0].
}

@defproc[(- (x0 number?) (x1 number?) ...) number?]{
 Computes the difference of @racket[x0] and @racket[x1 ...].
}

@defproc[(* (x number?) ...) number?]{
 Computes the product of @racket[x ...]. If no numbers are provided, returns @racket[1].
}

@defproc[(/ (x0 number?) (x1 number?) ...) number?]{
 If exactly one number @racket[x0] is provided, returns its reciprocal. Otherwise, the quotient of @racket[x0] and the product of @racket[x1 ...] is returned.
}

@defproc[(add1 (x number?)) number?]{
  Returns one greater than @racket[x].
}

@defproc[(sub1 (x number?)) number?]{
  Returns one less than @racket[x].
}

@section[#:tag "numeric-comparisons"]{Numeric Comparisons}

@defproc[(= (x0 number?) (x1 number?) (x2 number?) ...) boolean?]{
  Checks whether all arguments are equal numbers.
}

@defproc[(< (x0 number?) (x1 number?) (x2 number?) ...) boolean?]{
  Checks whether @racket[x0 x1 x2 ...] form a strictly increasing sequence of numbers.
}

@defproc[(> (x0 number?) (x1 number?) (x2 number?) ...) boolean?]{
  Checks whether @racket[x0 x1 x2 ...] form a strictly decreasing sequence of numbers.
}

@defproc[(<= (x0 number?) (x1 number?) (x2 number?) ...) boolean?]{
  Checks whether @racket[x0 x1 x2 ...] form an increasing sequence of numbers.
}

@defproc[(>= (x0 number?) (x1 number?) (x2 number?) ...) boolean?]{
  Checks whether @racket[x0 x1 x2 ...] form a decreasing sequence of numbers.
}


@section[#:tag "membership"]{Membership}

@defproc[(memv (needle any/c) (haystack list?)) (or/c #f list?)]{
 Returns the first pair of @racket[haystack] whose @racket[car] is @racket[eqv?] with @racket[needle], or @racket[#f] if no such pair exists in @racket[haystack].
}

@defproc[(assv (key any/c) (associations list?)) (or/c #f pair?)]{
 Returns the first pair found in @racket[associations] whose @racket[car] is @racket[eqv?] with @racket[key], or @racket[#f] if the key has no association in @racket[associations].
}

@section[#:tag "lists"]{Lists}

@defproc[(list? (v any/c)) boolean?]{
Checks whether @racket[v] is a proper list.
}

@defproc[(list-ref (haystack list?) (n number?)) any/c]{
Finds the @racket[n]th element of @racket[haystack], where the first element is @racket[0].
}

@defproc[(map (f procedure?) (list list?)) list?]{
  Apply @racket[f] to each element of @racket[list] in order, constructing a new list of the results.
}


@section[#:tag "functions"]{Functions}

@defproc[(apply (f procedure?) (arg any/c) ... (args list?)) any/c]{
  Apply @racket[f] to the argument list constructed from @racket[arg ...] and @racket[args].
} 

@defform*[((lambda (x ...) body)
           (lambda args body))]{
In the first form, construct a function that binds @racket[x ...] in @racket[body]. In the second form, construct a function that binds @racket[args] to the entire list of arguments. However, in Dan Scheme, the second form of @racket[lambda] must pass its argument list directly to a helper.
}

@defform*[((λ (x ...) body)
           (λ args body))]{
See @racket[lambda].
}


@section[#:tag "booleans"]{Booleans}

@defform[(and b ...)]{
If any of @racket[b ...] are @racket[#f], this form evaluates to @racket[#f]. Otherwise, it evaluates to the last @racket[b], or @racket[#t] if @racket[b ...] is empty.
}

@defform[(or b ...)]{
If all of @racket[b ...] are @racket[#f], this form evaluates to @racket[#f]. Otherwise, it evaluates to the first non-@racket[#f] @racket[b], or @racket[#f] if @racket[b ...] is empty.
}

@defproc[(not (b any/c)) boolean?]{
 Evaluates to @racket[#t] if @racket[b] is @racket[#f], or @racket[#f] otherwise.
}

@defproc[(boolean? (v any/c)) boolean?]{
  Evaluates to @racket[#t] if @racket[v] is either @racket[#t] or @racket[#f], or @racket[#f] if @racket[v] is anything else.
}

@defform[(if c t e)]{
 Evaluates to @racket[t] if @racket[c] is not @racket[#f], or @racket[e] otherwise.
}

@defform*[#:literals (else)
         ((cond (c e) ...)
          (cond (c e) ... (else e2)))]{
  Evaluates each @racket[c] in turn, until one is non-@racket{#f}. The result is the corresponding @racket{e}. If there is an @racket[else]-clause, it is used if non of the @racket[c ...] are non-@racket[#f].
}


@section[#:tag "symbols"]{Symbols}

@defproc[(symbol? (v any/c)) boolean?]{
 @racket[#t] if @racket[v] is a symbol, @racket[#f] otherwise.
}


@section[#:tag "binders"]{Definitions and Other Binders}

@defform*[((define x
             e)
           (define (f x ...)
             e))]{
  In the first form, binds @racket[x] to the value of @racket[e].
  The second form is equivalent to
  @racket[(define f (lambda (x ...) e))].
}

@defform[(let ((x e) ...) body)]{
  Binds each @racket[x ...] to the value of the corresponding @racket[e ...] in @racket[body].
}

@section[#:tag "Macros"]{Macros}

@defform[#:literals (syntax-rules)
         (define-syntax op
           (syntax-rules (literal ...) (pattern expr) ...))]{
  Associates the new special form @racket[op] with the macro defined by the @racket[syntax-rules] form. In Dan Scheme, @racket[define-syntax] must be followed by @racket[syntax-rules].
}

@defform[(syntax-rules (literal ...) (pattern stx) ...)]{
  Attempts to match each @racket[pattern] against the syntax being transformed, hygienically expanding to the corresponding @racket[stx] when one matches. The identifiers in @racket[literal ...] are matched directly as keywords.
}

@defidform[...]{
Specifies zero or more repetitions of a form.
}


@section[#:tag "modules"]{Modules}
Because simplicity is not @emph{always} to be valued over convenience, Dan Scheme supports importing Racket identifiers. See Racket's documentation for the meanings of @racket[require], @racket[provide], @racket[all-defined-out], and @racket[for-syntax].
