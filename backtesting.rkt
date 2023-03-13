#lang typed/racket

(require "kelly.rkt" "download-data.rkt" "analyze-data.rkt")


(: main (->* () ((Vectorof String)) Any))
(define main
  (λ ([argv (current-command-line-arguments)])
    (define start-date (string->milliseconds "2022-01-01"))
    (define mid-date   (string->milliseconds "2022-07-01"))
    (define end-date   (string->milliseconds "2023-01-01"))

    (define step (* 1000 60))

    (for/fold ([r* : (Listof Nonnegative-Real) '()]
               [margin : Real 10000])
              ([after  : Real (in-range mid-date end-date step)]
               [before : Real (in-range t start-date (- step))])
      
      )


    #t))
(module+ main (exit (main)))
