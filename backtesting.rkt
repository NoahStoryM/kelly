#lang typed/racket

(require typed/racket/date)
(require "analyze-data.rkt" "download-data.rkt")


(define $ 'BTC)

(: asset* (Listof (Pair Symbol (Boxof Real))))
(define asset* '())

(: get-total-value (-> Real))
(define get-total-value
  (λ ()
    (for/sum : Real ([asset (in-list asset*)])
      (define name (car asset))
      (define price (assert (assq name price*)))
      (* (unbox (cdr asset))
         (unbox (cdr price))))))

(: update-asset*! (-> (Listof (Pair Symbol Real)) Void))
(define update-asset*!
  (let ([fee : Real 0.0007])
    (λ (ratio*)
      (define total-value (get-total-value))
      (for ([ratio (in-list ratio*)])
        (define name (car ratio))
        (define f (cdr ratio))
        (define aim-value (* f total-value))
        (define price (assert (assq name price*)))
        (define aim-asset (/ aim-value (unbox (cdr price))))
        (define asset (assq name asset*))
        (cond
          [(false? asset)
           (: new-asset Real)
           (define new-asset (* aim-asset (- 1 fee)))
           (: b (Boxof Real))
           (define b (box new-asset))
           (define asset (cons name b))
           (set! asset* (cons asset asset*))]
          [else
           (define b (cdr asset))
           (define old-asset (unbox b))
           (define new-asset (- aim-asset (* fee (abs (- aim-asset old-asset)))))
           (set-box! b new-asset)])))))


(: price* (Listof (Pair Symbol (Boxof Real))))
(define price* `([,$ . ,(box-immutable (ann 1 Real))]))

(: update-price*! (-> (Listof Symbol) Integer Natural Void))
(define update-price*!
  (λ (name* ts bar)
    (for ([name : Symbol (in-list name*)])
      (define inst-id (cons name $))
      (define datum (assert (get-datum inst-id bar ts) list?))
      (define c (assert (list-ref datum 4) real?))
      (define price (assq name price*))
      (cond
        [(false? price)
         (: b (Boxof Real))
         (define b (box c))
         (define price (cons name b))
         (set! price* (cons price price*))]
        [else
         (define b (cdr price))
         (set-box! b c)]))))

(: main (->* () ((Vectorof String)) Any))
(define main
  (λ ([argv (current-command-line-arguments)])
    (define start-date (string->milliseconds "2022-01-01"))
    (define mid-date   (string->milliseconds "2022-06-01"))
    (define end-date   (string->milliseconds "2023-03-01"))

    (define inst-id* `([ETH . ,$] [LTC . ,$]))
    (define day* (dates-between start-date end-date))
    (set! asset* `([,$  . ,(box (ann 1 Real))]
                   [ETH . ,(box (ann 0 Real))]
                   [LTC . ,(box (ann 0 Real))]))
    (define step (* 150 60 1000))
    (parameterize ([bar 1])
      (load-jsexpr! (bar) inst-id* day*)
      (update-price*! #;name* '(ETH LTC) #;ts mid-date #;bar (bar))
      (define r*
        (for/fold ([r* : (Listof Nonnegative-Real) '()])
                  ([after : Integer (in-range mid-date end-date step)])
          (define old-total-value (get-total-value))
          (update-price*! #;name* '(ETH LTC) #;ts after #;bar (bar))

          (: aim-ratio* (Listof (Pair Symbol Real)))
          (define aim-ratio*
            (get-ratio*
             #;min-lever -003.50
             #;max-lever +004.50
             #;$ $
             #;name* '(ETH LTC)
             #;before start-date
             #;after  (add1 after)
             #;bar (bar)
             #;min-limit (* 20 24 60)
             #;max-limit (* 60 24 60)))

          (define old-ratio*
            (for/list : (Listof (Pair Symbol Real))
                      ([asset (in-list asset*)])
              (define name (car asset))
              (define price (assert (assq name price*)))
              (cons name
                    (/ (* (unbox (cdr asset))
                          (unbox (cdr price)))
                       old-total-value))))

          (define proportion
            (for/sum : Real ([aim-ratio (in-list aim-ratio*)])
              (define name (car aim-ratio))
              (define old-ratio (assert (assq name old-ratio*)))
              (define old-proportion (cdr old-ratio))
              (define aim-proportion (cdr aim-ratio))
              (abs (- old-proportion aim-proportion))))

          (define new-ratio* (if (> proportion 0.15) aim-ratio* old-ratio*))
          (when (eq? new-ratio* aim-ratio*)
            (displayln (format "Rebalance: delta proportion = ~a" proportion))
            (update-asset*! new-ratio*))

          (define new-total-value (get-total-value))
          (define r (/ new-total-value old-total-value))

          (displayln (format "Time: ~a (~a)" after (date->string (seconds->date (quotient after 1000)) #t)))
          (displayln (format "Prices: ~a" price*))
          (displayln (format "Assets: ~a" asset*))
          (displayln (format "Ratios: ~a" new-ratio*))
          (displayln (format "Total value: ~a ₿" new-total-value))
          (displayln (format "Rate: ~a" r))
          (newline)

          (if (positive? r)
              (cons r r*)
              (error 'backtesting "爆仓！"))))
      (displayln (format "Total rate: ~a" (apply * r*))))

    #t))
(module+ main (exit (main)))
