#lang typed/racket

(provide (all-defined-out))


(define-type Asset (Pair Symbol (Boxof Real)))

(: asset* (Listof Asset))
(define asset* '())

#|TODO
(: total-value (-> Real))
(define total-value (λ () ))
|#


(define-type Position (List Symbol (Boxof Real) (Boxof Real)))

(: position* (Listof Position))
(define position* '())

(: get-position*  (-> (Listof Position)))
(define get-position* (λ () position*))

(: adjust-position! (-> Symbol Real Void))
(define (adjust-position! name cash)
  (when (>= (abs cash) 0.01)
    (cond
      [(positive? cash) (displayln (format "开仓：~a，仓位增加：~a $" name cash))]
      [(negative? cash) (displayln (format "平仓：~a，仓位减少：~a $" name (- cash)))])

    (define position (assert (assq position* name) list?))
    (define lever (cadr  position))
    (define imr   (caddr position))
    (set-box! imr (+ (unbox imr) (/ cash (abs (unbox lever)))))

    ;; TODO
    #;(define asset (assert (assq asset* name) list?))))

(: adjust-lever! (-> Symbol Real Void))
(define (adjust-lever! name lever)
  (displayln (format "杠杆：~a，倍数：~a x" name lever)))


(: allocation->position*
   (-> Real
       (-> (Listof (Pair Symbol Real))
           (Listof Position))))
(define allocation->position*
  (λ (margin)
    (λ (allocation)
      (define x (length allocation))
      (for/list ([f : (Pair Symbol Real) (in-list allocation)])
        (list (car f)
              (* x (cdr f))
              (exact->inexact (/ margin x)))))))


(: balance-position!
   (-> Real
       (Listof Position)
       (Listof Position)
       Void))
(define (balance-position! margin old-position* new-position*)
  (: now-position* (Listof Position))
  (define now-position*
    ;; Release imr.
    (for/fold ([now-position* : (Listof Position) '()])
              ([old-position  : Position (in-list old-position*)])
      (match old-position
        [`(,name ,old-lever ,old-imr)
         #:when (and (symbol? name)
                     (real? old-lever)
                     (real? old-imr))
         (define old-cash (abs (* old-lever old-imr)))
         (match (assq name new-position*)
           [#f ;; Close irrelevant positions.
            (close-position! name +inf.0 #;old-cash)
            (set! margin (+ margin old-imr))
            now-position*]
           [`(,_ ,new-lever ,new-imr)
            #:when (and (real? new-lever)
                        (real? new-imr))
            (define new-cash (abs (* new-lever new-imr)))
            (cond
              [(or ;; Close opposite positions.
                (and (negative? old-lever) (positive? new-lever))
                (and (positive? old-lever) (negative? new-lever)))
               (close-position! name +inf.0 #;old-cash)
               (set! margin (+ margin old-imr))
               now-position*]
              [(< (abs old-lever) (abs new-lever)) ; Turn up lever.
               (adjust-lever! name new-lever)
               (define now-imr (/ old-cash  new-lever))
               (define imr     (- new-imr   now-imr))
               (cond
                 [(>= margin imr)
                  (adjust-position! name (- new-cash old-cash))
                  (set! margin (- margin imr))
                  (: now-position Position)
                  (define now-position
                    (list name new-lever new-imr))
                  (cons now-position now-position*)]
                 [else
                  (: now-position Position)
                  (define now-position
                    (let ([now-imr (+ now-imr margin)])
                      (list name new-lever now-imr)))
                  (cons now-position now-position*)])]
              [else
               (define now-imr (/ new-cash  old-lever))
               (define imr     (- old-imr   now-imr))
               (cond
                 [(positive? imr)   ; Check if old cash is higher than new cash.
                  (close-position! name (- old-cash new-cash))
                  (set! margin (+ margin imr))
                  (: now-position Position)
                  (define now-position (list name old-lever now-imr))
                  (cons now-position now-position*)]
                 [else (cons old-position now-position*)])])])])))
  ;; Turn down lever and invest margin.
  (for ([new-position (in-list new-position*)])
    (match new-position
      [`(,name ,new-lever ,new-imr)
       #:when (and (symbol? name)
                   (real? new-lever)
                   (real? new-imr))
       (define new-cash (abs (* new-lever new-imr)))
       (match (assq name now-position*)
         [#f ;; Open relevant positions.
          (adjust-lever! name new-lever)
          (open-position! name new-cash)
          (set! margin (- margin new-imr))]
         [`(,_ ,now-lever ,now-imr)
          #:when (and (real? now-lever)
                      (real? now-imr))
          (define now-cash (abs (* now-lever now-imr)))
          (define imr
            (cond
              [(< (abs new-lever) (abs now-lever)) ; Turn down lever
               (adjust-lever! name new-lever)
               (let* ([now-cash (* now-lever now-imr)]
                      [now-imr  (/ now-cash  new-lever)])
                 (- new-imr now-imr))]
              [else (- new-imr now-imr)]))
          (open-position! name (- new-cash now-cash))
          (set! margin (- margin imr))])])))
