#lang typed/racket

(require "kelly.rkt" "download-data.rkt")
(provide (all-defined-out))

(: get-margin  (-> Real))
(: set-margin! (-> Real Void))
(define-values (get-margin set-margin!)
  (let ([margin : Real 0])
    (values
     (λ () margin)
     (λ (v) (set! margin v)))))

(: get-position*  (-> (Listof (List Symbol Real Real))))
(: set-position*! (-> (Listof (List Symbol Real Real)) Void))
(define-values (get-position* set-position!)
  (let ([position* : (Listof (List Symbol Real Real)) '()])
    (values
     (λ () position*)
     (λ (v) (set! position* v)))))

(: allocation->position*
   (-> Real
       (-> (Listof (Pair Symbol Real))
           (Listof (List Symbol Real Real)))))
(define allocation->position*
  (λ (margin)
    (λ (allocation)
      (define x (length allocation))
      (for/list ([f : (Pair Symbol Real) (in-list allocation)])
        (list (car f)
              (* x (cdr f))
              (exact->inexact (/ margin x)))))))


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
(define (display-msg day bar total-lever allocation)
  (newline)
  (displayln (format "Date: ~a" day))
  (displayln (format "Bar: ~a" bar))
  (displayln (format "Total Lever: ~a x" total-lever))

  (displayln "Allocation:")
  (pretty-print allocation)

  ;; TODO Cancel orders.

  (define margin (* 0.95 5000))
  (define old-position* (get-position*))
  (define new-position* ((allocation->position* margin) allocation))
  (displayln "New positions:")
  (pretty-print new-position*)

  (balance-position! margin old-position* new-position*))

(: main (-> String * Any))
(define main
  (λ argv
    (displayln #"Start analyzing data...")
    (newline)

    (: min-lever Real)
    (define min-lever 3.5)

    (: max-lever Real)
    (define max-lever 14.5)

    (define end-date (today))
    (define dates (reverse (dates-between "2023-01-01" end-date)))
    (time
     (let loop : Void
          ([day* : (Listof String) dates]
           [bar* : (Listof Natural) '(1 3 5 15 30 60 120 240 360 720 1440)]
           [min-day : String ""]
           [min-allocation : (Listof (Pair Symbol Real)) '()]
           [min-total-lever : Real +inf.0])
       (parameterize ([bar (car bar*)])
         (define start-date (car day*))
         (define before (string->milliseconds start-date))
         (define after  (string->milliseconds end-date))

         (define allocation
           (kelly 'BTC #;(USD)
                  '(#;BTC ETH LTC #;BNB #;OKB #;DYDX #;SOL #;OKT)
                  (bar) before after))

         (: total-lever Real)
         (define total-lever
           (for/sum : Real ([p : (Pair Symbol Real) (in-list allocation)])
             (max 0 (cdr p))))

         (displayln (format "date = ~a; bar = ~a; lever = ~a x" start-date (bar->string (bar)) total-lever))
         #;(pretty-print allocation)
         (newline)

         (cond
           [(<= min-lever total-lever max-lever)
            (display-msg start-date (bar->string (bar)) total-lever allocation)]
           [(null? (cdr day*))
            (cond
              [(null? (cdr bar*))
               (display-msg min-day (bar->string (bar)) min-total-lever min-allocation)]
              [(< total-lever min-total-lever)
               (loop dates (cdr bar*) start-date allocation total-lever)]
              [else
               (loop dates (cdr bar*) min-day min-allocation min-total-lever)])]
           [(< total-lever min-total-lever)
            (loop (cdr day*) bar* start-date allocation total-lever)]
           [else
            (loop (cdr day*) bar* min-day min-allocation min-total-lever)]))))

    #;(newline)
    #;(sleep 60)
    #;(main)
    #t))
(module+ main (exit (main)))
