#lang typed/racket

(require typed/json math)
(require "download-data.rkt" "gbm.rkt")

(provide kelly)

(: t Nonnegative-Real)
(define t 1)


(: kelly (-> Symbol (Listof Symbol) Natural Integer Integer (Listof (Pair Symbol Real))))
(define kelly
  (λ ($ name* bar before after)
    (let/cc return : (Listof (Pair Symbol Real))
      (define AB*
        (for/vector : (Vectorof (Vector Real Real))
                    ([name : Symbol (in-list name*)])
          (define inst-id (cons name $))
          (define r* (get-r* inst-id bar before after))
          (when (null? r*) (return '()))
          (define-values (μ σ^2) (gbm t r*))
          (define e^μ (exp μ))
          (define B (sub1 e^μ))
          (define A (- (* e^μ e^μ (exp σ^2)) (* 2 e^μ) -1))
          (vector A B)))

      (define len (length name*))

      (define M
        (build-matrix
         len len
         (ann
          (λ (m n)
            (define cm (vector-ref AB* m))
            (define cn (vector-ref AB* n))
            (if (= m n)
                (vector-ref cm 0)
                (* (vector-ref cm 1)
                   (vector-ref cn 1))))
          (-> Index Index Real))))

      (define B
        (build-matrix
         len 1
         (ann
          (λ (m n)
            (define cm (vector-ref AB* m))
            (vector-ref cm 1))
          (-> Index Index Real))))

      (define f* (array->list (matrix-solve M B)))

      (return
       (for/list : (Listof (Pair Symbol Real))
                 ([f : Real (in-list (cons (- 1 (apply + f*)) f*))]
                  [name : Symbol (in-list (cons $ name*))])
         (cons name f))))))
