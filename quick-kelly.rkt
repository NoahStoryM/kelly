#lang typed/racket

(require typed/json math)
(require "download-data.rkt")
(require/typed "gbm.rkt"
  [gbm (-> Nonnegative-Real (Listof Nonnegative-Real)
           (Values Real Nonnegative-Real))])

(provide kelly)


(: cache (Mutable-HashTable Path-String Boolean #;(Listof Any)))
(define cache (make-hash))

(: s*:15m (Mutable-HashTable (Pair Symbol Symbol) (Listof Nonnegative-Real)))
(define s*:15m (make-hash))

(: t Nonnegative-Real)
(define t 1)

(: kelly (-> Symbol
             (Listof Symbol)
             (Listof String)
             (Listof (Pair Symbol Real))))
(define kelly
  (λ ($ name* dates)
    (define AB*
      (for/vector : (Vectorof (Vector Real Real))
                  ([name : Symbol (in-list name*)])
        (define inst-id (cons name $))
        (define s*
          (for/fold ([s* : (Listof Nonnegative-Real) (hash-ref! s*:15m inst-id (λ () '()))]
                     #:result (begin0 s* (hash-set! s*:15m inst-id s*)))
                    ([day : String (in-list (reverse dates))])
            (define path (format "data/~a-~a-trades-~a.json" name $ day))
            (cond
              [(hash-has-key? cache path) s*]
              [else
               (unless (file-exists? path)
                 (download-daily-data (list inst-id) (list day) #:history? (not (today? day))))
               (let* ([json (call-with-input-file path read-json)]
                      [json (assert json hash?)]
                      [data (hash-ref json 'data)]
                      [data (assert data list?)]
                      [data
                       (if (today? day)
                           (let* ([datum (list-ref data 0)]
                                  [datum (assert datum list?)]
                                  [milliseconds (list-ref datum 0)]
                                  [milliseconds (assert milliseconds real?)])
                             (if (>= (- (current-milliseconds) milliseconds) 900000 #;(* 1000 60 15))
                                 (begin
                                   (download-daily-data (list inst-id) (list day) #:history? #f)
                                   (let* ([json (call-with-input-file path read-json)]
                                          [json (assert json hash?)]
                                          [data (hash-ref json 'data)]
                                          [data (assert data list?)])
                                     data))
                                 data))
                           data)])
                 (hash-set! cache path #t #;data)
                 (for/fold ([s* s*])
                           ([datum (in-list data)])
                   (let* ([datum (assert datum list?)]
                          [s (list-ref datum 4)]
                          [s (assert s real?)]
                          [s (assert s positive?)])
                     (cons s s*))))])))
        (define-values (μ σ^2) (gbm t s*))
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

    (for/list ([f : Real (in-list (cons (- 1 (apply + f*)) f*))]
               [name : Symbol (in-list (cons $ name*))])
      (cons name f))))
