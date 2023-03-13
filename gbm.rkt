#lang typed/racket

(provide gbm)

(: gbm (-> Nonnegative-Real (Listof Nonnegative-Real) (Values Real Nonnegative-Real)))
(define gbm
  (λ (t r*)
    (define-values (dt rt ss)
      (for*/fold ([dt : Nonnegative-Real 0]
                  [rt : Nonnegative-Real 1]
                  [ss : Nonnegative-Real 0])
                 ([r  : Nonnegative-Real (in-list r*)])
        (define s (sqr (sub1 r)))
        (values (+ dt t) (* rt r) (+ ss s))))
    (define lnr (/ (log rt) dt))
    (define σ^2 (assert (/ ss dt) positive?))
    (define μ (+ (/ σ^2 2) lnr))
    (values μ σ^2)))


(: main (->* () ((Vectorof String)) Any))
(define main
  (λ ([argv (current-command-line-arguments)])
    (let ([t 1/12])
      (: s* (Listof Positive-Real))
      (define s* '(100 120 140 160 180 200 210 180 200 160 180 200 210 160 180 200 210))
      (define r* (for/list : (Listof Nonnegative-Real) ([s0 (in-list s*)] [s1 (in-list (cdr s*))]) (/ s1 s0)))
      (define-values (μ σ^2) (gbm t r*))
      (displayln (format "t = ~a" t))
      (displayln (vector μ (sqrt σ^2) (/ μ σ^2)))
      (newline))
    #t))
(module+ main (exit (main)))
