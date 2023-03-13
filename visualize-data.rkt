#lang racket

(require "download-data.rkt")
(require json plot)
(plot-new-window? #t)

(define avg (λ n* (/ (apply + n*) (length n*))))

(define (visualize-data label jsexpr)
  (define code (hash-ref jsexpr 'code))
  (define data (reverse (hash-ref jsexpr 'data)))
  (define msg  (hash-ref jsexpr 'msg))
  (define rate*
    (for/list ([old-datum (in-list data)]
               [new-datum (in-list (cdr data))]
               [n (in-naturals)])
      #;(match-define `(,ts ,o ,h ,l ,c ,vol ,volCcy ,volCcyQuote ,confirm) datum)
      #;(define unit-time (* 1000 60 60 4))

      #;(define old-ts (/ (list-ref old-datum 0) unit-time))
      (define old-v (list-ref old-datum 4))

      #;(define new-ts (/ (list-ref new-datum 0) unit-time))
      (define new-v (list-ref new-datum 4))

      (define r (sub1 (/ new-v old-v)))
      (vector n r)))
  (plot (list (axes) (lines rate* #:label label))))


(define main
  (λ ([argv (current-command-line-arguments)])
    (let ()
      (download-data #"2H" '([ETH . BTC] [LTC . BTC] [SOL . BTC]))
      (begin
        (define ETH-BTC (call-with-input-file "data/ETH-BTC.json" read-json))
        (visualize-data "ETH-BTC" ETH-BTC))
      (begin
        (define LTC-BTC (call-with-input-file "data/LTC-BTC.json" read-json))
        (visualize-data "LTC-BTC" LTC-BTC))
      (begin
        (define SOL-BTC (call-with-input-file "data/SOL-BTC.json" read-json))
        (visualize-data "SOL-BTC" SOL-BTC)))

    (let ()
      (download-data #"2H" '([BTC . USDC] [ETH . USDC] [LTC . USDC] [SOL . USDC]))
      (begin
        (define BTC-USDC (call-with-input-file "data/BTC-USDC.json" read-json))
        (visualize-data "BTC-USDC" BTC-USDC))
      (begin
        (define ETH-USDC (call-with-input-file "data/ETH-USDC.json" read-json))
        (visualize-data "ETH-USDC" ETH-USDC))
      (begin
        (define LTC-USDC (call-with-input-file "data/LTC-USDC.json" read-json))
        (visualize-data "LTC-USDC" LTC-USDC))
      (begin
        (define SOL-USDC (call-with-input-file "data/SOL-USDC.json" read-json))
        (visualize-data "SOL-USDC" SOL-USDC)))
    #t))
(module+ main (exit (main)))
