#lang typed/racket

(require typed/racket/date)
(require "analyze-data.rkt" "download-data.rkt")
(provide (all-defined-out)
         (all-from-out "analyze-data.rkt")
         (all-from-out "download-data.rkt"))


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
  (let ([fee : Real (/ 0.0009 #;0.0007 2)])
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
          [asset
           (define b (cdr asset))
           (define old-asset (unbox b))
           (define new-asset (- aim-asset (* fee (abs (- aim-asset old-asset)))))
           (set-box! b new-asset)]
          [else
           (: new-asset Real)
           (define new-asset (* aim-asset (- 1 fee)))
           (: b (Boxof Real))
           (define b (box new-asset))
           (define asset (cons name b))
           (set! asset* (cons asset asset*))])))))


(: price* (Listof (Pair Symbol (Boxof Real))))
(define price* '())

(: update-price*! (-> Symbol (Listof Symbol) Integer Natural Void))
(define update-price*!
  (λ ($ name* ts bar)
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

(: eval-base-ratio* (-> Real (Listof (Pair Symbol Real)) (Listof (Pair Symbol Real))))
(define eval-base-ratio*
  (λ (#;expected-total-lever etl #;kelly-ratio* kr*)
    (define #;actual-total-lever atl
      (for/sum : Real ([kr (in-list kr*)])
        (max 0 (cdr kr))))
    (for/list : (Listof (Pair Symbol Real))
              ([kr (in-list kr*)])
      (define name (car kr))
      (let* ([f (max 0 (/ (cdr kr) atl))]
             [f (min etl f)])
        (cons name f)))))

(: get-inst-id* (-> Symbol (Listof Symbol) (Listof (Pair Symbol Symbol))))
(define get-inst-id*
  (λ ($ name*)
    (for/list : (Listof (Pair Symbol Symbol))
              ([name (in-list name*)])
      (cons name $))))

(: test (-> Real Symbol (Listof Symbol) Integer Integer Integer Natural Natural
            (Values Nonnegative-Real Nonnegative-Real Nonnegative-Real)))
(define test
  (λ (lever $ name* start-date mid-date end-date step days)
    (define inst-id* (get-inst-id* $ name*))
    (set! asset*
          (cons
           (cons $ (box (ann 1 Real)))
           (for/list : (Listof (Pair Symbol (Boxof Real)))
                     ([name (in-list name*)])
             (: b (Boxof Real))
             (define b (box 0))
             (cons name b))))
    (set! price* `([,$ . ,(box-immutable (ann 1 Real))]))
    (update-price*! #;$ $ #;name* name* #;ts mid-date #;bar (bar))
    (define r*
      (for/list : (Listof Nonnegative-Real)
                ([after : Integer (in-range mid-date end-date step)])
        (define old-total-value (get-total-value))
        (update-price*! #;$ $ #;name* name* #;ts after #;bar (bar))
        (define old-ratio*
          (let ([total-value (get-total-value)])
            (for/list : (Listof (Pair Symbol Real))
                      ([asset (in-list asset*)])
              (define name (car asset))
              (define price (assert (assq name price*)))
              (cons name
                    (/ (* (unbox (cdr asset))
                          (unbox (cdr price)))
                       total-value)))))

        (: aim-ratio* (Listof (Pair Symbol Real)))
        (define aim-ratio*
          (eval-base-ratio*
           lever
           (get-ratio*
            #;min-lever -inf.0
            #;max-lever +inf.0
            #;$ $
            #;name* name*
            #;before start-date
            #;after  (add1 after)
            #;bar (bar)
            #;min-days days
            #;max-days days)))

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

        (define day (date->string (seconds->date (quotient after 1000)) #t))
        (displayln (format "Time: ~a (~a)" after day))
        (displayln "Prices:") (pretty-print price*)
        (displayln "Assets:") (pretty-print asset*)
        (displayln "Ratios:") (pretty-print new-ratio*)
        (displayln (format "Total value: ~a ~a" new-total-value $))
        (displayln (format "Rate: ~a" r))
        (newline)

        (if (positive? r) r (error 'backtesting "~a: 爆仓！" day))))

    (for/fold ([total-r : Nonnegative-Real 1]
               [min-r   : Nonnegative-Real 1]
               [max-r   : Nonnegative-Real 1])
              ([r (in-list r*)])
      (let* ([total-r (* r total-r)]
             [min-r   (min min-r total-r)]
             [max-r   (max max-r total-r)])
        (values total-r min-r max-r)))))

(: main (->* () ((Vectorof String)) Any))
(define main
  (λ ([argv (current-command-line-arguments)])
    (displayln #"Start main work...")

    (define $ 'BTC)
    (define name* '(#;BTC ETH LTC XMR DASH OKB BNB SOL APT LINK DYDX UNI))
    (define inst-id* (get-inst-id* $ name*))


    (define start-date (string->milliseconds "2023-01-01"))
    (define mid-date   (string->milliseconds "2023-01-17"))
    (define end-date   #;(current-milliseconds) (string->milliseconds "2023-03-20"))
    (define day* (dates-between start-date end-date))
    (parameterize ([bar 1])
      (time (load-jsexpr! (bar) inst-id* day*))

      (call-with-output-file "main.log"
        #:exists 'truncate/replace
        (ann
         (λ (out)
           (define res
             (for*/list : (Listof (Immutable-Vector
                                   Positive-Integer Positive-Integer Real
                                   Nonnegative-Real Nonnegative-Real Nonnegative-Real))
                        ([bar   : Positive-Integer (in-list '(1440) #;'(60 120 360 720 1440))]
                         [days  : Positive-Integer (in-list '(16)) #;(in-range 14 21)]
                         [lever : Real (in-list '(1))])
               (time
                (displayln (format "bar = ~a, days = ~a, lever = ~a." bar days lever))
                (define step (* bar 60 1000))
                (define-values (min-r max-r total-r)
                  (parameterize (#;[current-output-port out])
                    (displayln (format "bar = ~a, days = ~a, lever = ~a." bar days lever))
                    (test lever $ name* start-date mid-date end-date step days)))
                (vector-immutable bar days lever min-r max-r total-r))))
           (pretty-print res))
         (-> Output-Port Any))))

    #t))
(module+ main (exit (main)))
