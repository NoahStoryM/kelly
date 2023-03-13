#lang racket

(require file/unzip csv-reading)
(require racket/date (only-in srfi/19 string->date))
(date-display-format 'iso-8601)

(provide gbm read-data)

(define avg (λ (n . n*) (let ([n* (cons n n*)]) (/ (apply + n*) (length n*)))))
(define string->seconds (λ (str) (date->seconds (string->date str "~Y-~m-~d") #f)))
(define seconds->string (λ (sec) (date->string (seconds->date sec) #f)))
(define dates-between
  (case-lambda
    [()  (seconds->string (current-seconds))]
    [(s) (dates-between s (dates-between))]
    [(s e)
     (for/list ([cur (in-inclusive-range
                      (string->seconds s)
                      (string->seconds e)
                      (* 60 60 24))])
       (seconds->string cur))]))

(define read-data
  (λ (spot dates)
    (parameterize ([current-directory "data"])
      (define date->ts*
        (λ (d)
          (define trades-name (format "~a-trades-~a" spot d))
          (define csv-path (format "~a.csv" trades-name))
          (unless (file-exists? csv-path)
            (define zip-path (format "~a.zip" trades-name))
            (unless (file-exists? zip-path)
              (define download-link
                (format
                 "https://static.okx.com/cdn/okex/traderecords/trades/daily/~a/~a.zip"
                 (string-replace d "-" "")
                 trades-name))
              (system (format "aria2c -x8 '~a'" download-link)))
            (unzip zip-path)
            (delete-file zip-path))
          (cdr
           (csv-map
            (λ (line)
              (vector
               (string->number (list-ref line 4))
               (string->number (list-ref line 3))))
            (open-input-file csv-path)))))
      (for*/list ([d (in-list dates)] [ts (in-list (date->ts* d))]) ts))))


(define gbm
  (let ()
    (define >?
      (λ (ts1 ts2)
        (> (vector-ref ts1 0)
           (vector-ref ts2 0))))
    (λ (t ts*)
      (define-values (t* s*)
        (for/fold ([t* '()] [s* '()])
                  ([ts (in-list (sort ts* >?))])
          (values
           (cons (vector-ref ts 0) t*)
           (cons (vector-ref ts 1) s*))))
      (pretty-print
       (map
        (λ (t)
          (exact->inexact (- (/ t 1000) (string->seconds (seconds->string (/ t 1000))))))
        t*))
      (newline)

      (define r* (for/list ([s0 (in-list s*)] [st (in-list (cdr s*))]) (/ st s0)))
      (define lnr (/ (log (/ (last s*) (first s*))) (- (last t*) (first t*))))
      (define σ^2 (/ (apply avg (for/list ([r (in-list r*)]) (sqr (sub1 r)))) t))
      (define μ (+ (/ σ^2 2) lnr))
      (values μ σ^2))))


(define main
  (λ argv
    (let ([t 1/12])
      (define s* '(100 120 140 160 180 200 210 180 200 160 180 200 210 160 180 200 210))
      (define t* (build-list (length s*) (curry * t)))
      (define ts* (map vector t* s*))
      (define-values (μ σ^2) (gbm t ts*))
      (displayln (vector μ σ^2 (/ μ σ^2)))
      (newline))
    (let ([t 1 #;(/ 1 1000 60 60 24)])
      (define ts* (read-data "BTC-USDC" (dates-between "2023-02-10" #;"2023-02-10")))
      (define-values (μ σ^2) (gbm t ts*))
      (displayln (vector μ σ^2 (/ μ σ^2)))
      (newline))))
(module+ main (call-with-values (λ () (main)) exit))
