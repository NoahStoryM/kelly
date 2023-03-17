#lang typed/racket

(require typed/racket/date)
(require "kelly.rkt" "download-data.rkt")
(provide (all-defined-out))


(: ratio*->position*
   (-> Real
       (-> (Listof (Pair Symbol Real))
           (Listof (List Symbol Real Real)))))
(define ratio*->position*
  (λ (margin)
    (λ (ratio*)
      (define x (length ratio*))
      (for/list ([ratio : (Pair Symbol Real) (in-list ratio*)])
        (define name (car ratio))
        (define proportion (cdr ratio))
        (list name (* proportion x) (exact->inexact (/ margin x)))))))


(: open-position! (-> Symbol Real Void))
(define (open-position! name cash)
  (let ([cash (abs cash)])
    (when (>= cash 0.01)
      (displayln (format "开仓：~a，仓位：~a $" name cash)))))

(: close-position! (-> Symbol Real Void))
(define (close-position! name cash)
  (let ([cash (abs cash)])
    (when (>= cash 0.01)
      (displayln (format "平仓：~a，仓位：~a $" name cash)))))

(: adjust-position! (-> Symbol Real Void))
(define (adjust-position! name cash)
  (cond
    [(positive? cash) (open-position!  name cash)]
    [(negative? cash) (close-position! name cash)]))

(: adjust-lever! (-> Symbol Real Void))
(define (adjust-lever! name lever)
  (displayln (format "杠杆：~a，倍数：~a x" name lever)))


(: balance-position!
   (-> Real
       (Listof (List Symbol Real Real))
       (Listof (List Symbol Real Real))
       Void))
(define (balance-position! margin old-position* new-position*)
  (: now-position* (Listof (List Symbol Real Real)))
  (define now-position*
    ;; Release imr.
    (for/fold ([now-position* : (Listof (List Symbol Real Real)) '()])
              ([old-position  : (List Symbol Real Real) (in-list old-position*)])
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
                  (: now-position (List Symbol Real Real))
                  (define now-position
                    (list name new-lever new-imr))
                  (cons now-position now-position*)]
                 [else
                  (: now-position (List Symbol Real Real))
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
                  (: now-position (List Symbol Real Real))
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

(: get-ratio* (-> Real Real
                  Symbol (Listof Symbol)
                  Integer Integer
                  Natural Natural Natural
                  (Listof (Pair Symbol Real))))
(define get-ratio*
  (λ (min-lever max-lever $ name* before after bar min-limit max-limit)
    (define end-date (milliseconds->string after))
    (define min-days (quotient min-limit (* 60 24)))
    (define max-days (quotient max-limit (* 60 24)))
    (let loop : (Listof (Pair Symbol Real))
         ([day* : (Listof String)
                (for/list ([day (in-list (reverse (dates-between before after)))]
                           [i (in-naturals)]
                           #:when (<= min-days i max-days))
                  day)]
          [min-day : String ""]
          [min-ratio* : (Listof (Pair Symbol Real)) '()]
          [min-total-lever : Real +inf.0])
      (define start-date (car day*))
      (define before (string->milliseconds start-date))
      (define ratio* (kelly $ name* bar before after))

      (: total-lever Real)
      (define total-lever
        (for/sum : Real ([p : (Pair Symbol Real) (in-list ratio*)])
          (max 0 (cdr p))))

      #;(pretty-print ratio*)

      (cond
        [(and (pair? ratio*)
              (andmap
               (ann (λ (p) (<= min-lever (cdr p) max-lever))
                    (-> (Pair Symbol Real) Boolean))
               ratio*))
         (displayln (format "date : ~a -> ~a;\nbar = ~a;\nlever = ~a x"
                            (date->string (seconds->date (quotient before 1000)) #t)
                            (date->string (seconds->date (quotient after  1000)) #t)
                            (bar->string bar) total-lever))
         #;(display-msg start-date (bar->string bar) total-lever ratio*)
         ratio*]
        [(null? (cdr day*))
         (displayln (format "date : ~a -> ~a;\nbar = ~a;\nlever = ~a x"
                            (date->string (seconds->date (quotient before 1000)) #t)
                            (date->string (seconds->date (quotient after  1000)) #t)
                            (bar->string bar) min-total-lever))
         #;(display-msg min-day (bar->string bar) min-total-lever min-ratio*)
         min-ratio*]
        [(or (null? ratio*) (>= total-lever min-total-lever))
         (loop (cdr day*) min-day min-ratio* min-total-lever)]
        [else
         (loop (cdr day*) start-date ratio* total-lever)]))))


(: get-position* (-> (Listof (List Symbol Real Real))))
(define get-position*
  (λ ()
    '(#;[BTC  +00.00 0000.00]
      #;[ETH  +00.00 0000.00]
      #;[LTC  +00.00 0000.00]
      #;[BNB  +00.00 0000.00]
      #;[OKB  +00.00 0000.00]
      #;[SOL  +00.00 0000.00])))

(: display-msg (-> String String Real (Listof (Pair Symbol Real)) Void))
(define (display-msg day bar total-lever ratio*)
  (newline)
  (displayln (format "Date: ~a" day))
  (displayln (format "Bar: ~a" bar))
  (displayln (format "Total Lever: ~a x" total-lever))

  (displayln "Ratio*:")
  (pretty-print ratio*)

  ;; TODO Cancel orders.

  (define margin (* 0.95 5000))
  (define old-position* (get-position*))
  (define new-position* ((ratio*->position* margin) ratio*))
  (displayln "New positions:")
  (pretty-print new-position*)

  (balance-position! margin old-position* new-position*))

(: main (-> String * Any))
(define main
  (λ argv
    (displayln #"Start analyzing data...")
    (newline)

    (pretty-print
     (time
      (parameterize ([bar 1])
        (get-ratio*
         #;min-lever -003.50
         #;max-lever +004.50
         #;$ 'BTC
         #;name* '(ETH LTC OKB BNB DYDX UNI #;LINK #;APT #;SOL)
         #;before (string->milliseconds "2022-01-01")
         #;after  (assert (current-milliseconds) exact-integer?)
         #;bar (bar)
         #;min-limit (*  7 24 60)
         #;max-limit (* 21 24 60)))))

    #;(newline)
    #;(sleep 60)
    #;(main)
    #t))
(module+ main (exit (main)))
