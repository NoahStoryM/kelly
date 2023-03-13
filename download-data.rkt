#lang typed/racket

(require typed/json typed/net/url typed/racket/date)
(require/typed srfi/19 [string->date (-> String String date)])

(provide simple-download download
         download-data download-daily-data download-daily-data*
         update-data dates-between
         seconds->string milliseconds->string
         string->seconds string->milliseconds
         minutes-elapsed-since-midnight
         USD bar bar->string
         today today?
         get-r*)

(define-predicate jsexpr-number? (U Integer Inexact-Real))

(: string->jsexpr-number (-> String (U Integer Inexact-Real)))
(define string->jsexpr-number (λ (str) (assert (string->number str) jsexpr-number?)))

(: string->seconds (-> String [#:format-string String] Integer))
(define string->seconds
  (λ (str #:format-string [fmt-str "~Y-~m-~d"])
    (date->seconds (string->date str fmt-str))))

(: seconds->string
   (->* (Real)
        (Any
         (U 'american
            'chinese
            'german
            'indian
            'irish
            'iso-8601
            'julian
            'rfc2822))
        String))
(define seconds->string
  (λ (secs [time? #f] [fmt 'iso-8601])
    (parameterize ([date-display-format fmt])
      (date->string (seconds->date secs) time?))))

(: string->milliseconds (-> String [#:format-string String] Integer))
(define string->milliseconds
  (λ (str #:format-string [fmt-str "~Y-~m-~d"])
    (* 1000 (string->seconds str #:format-string fmt-str))))

(: milliseconds->string
   (->* (Real)
        (Any
         (U 'american
            'chinese
            'german
            'indian
            'irish
            'iso-8601
            'julian
            'rfc2822))
        String))
(define milliseconds->string (λ (msecs [time? #f] [fmt 'iso-8601]) (seconds->string (/ msecs 1000) time? fmt)))

(: today (-> String))
(define today (λ () (seconds->string (current-seconds))))

(: today? (-> String Boolean))
(define today? (λ (day) (string=? day (today))))

(: minutes-elapsed-since-midnight (-> String Natural))
(define minutes-elapsed-since-midnight
  (λ (day)
    (define t0 (string->seconds day))
    (define t1 (current-seconds))
    (define dt (- t1 t0))
    (cond
      [(negative? dt) 0]
      [(< dt #;1D=_s 86400) (quotient dt 60)]
      [else #;1D=_m 1440])))

(: dates-between
   (case->
    (-> (List String))
    (-> (U Integer String) (Listof String))
    (-> (U Integer String) (U Integer String) (Listof String))))
(define dates-between
  (let ([step #;1D=_s 86400000])
    (case-lambda
      [()  (list (seconds->string (current-seconds)))]
      [(s) (dates-between s (car (dates-between)))]
      [(s e)
       (let ([s (if (string? s) (string->milliseconds s) s)]
             [e (+ (if (string? e) (string->milliseconds e) e) step)])
         (for/list : (Listof String)
                   ([cur : Real (in-range s e step)])
           (milliseconds->string cur)))])))


(: USD (Parameter (U 'USD 'USDK 'USDC 'USDT 'DAI)))
(define USD (make-parameter 'USDC))

(: bar (Parameter Natural))
(define bar (make-parameter 15))

(: bar->string (-> Natural String))
(define bar->string
  (λ (bar)
    (case bar
      [(1 3 5 15 30) (~a bar 'm)]
      [(60)   "1H"]
      [(120)  "2H"]
      [(240)  "4H"]
      [(360)  "6H"]
      [(720)  "12H"]
      [(1440) "1D"]
      [else (error 'bar->string "invalid value: bar = ~a" bar)])))

(: data:name-USD->name-$ (-> (Listof JSExpr) (Listof JSExpr) (Listof JSExpr)))
(define data:name-USD->name-$
  (λ (data:name-USD data:$-USD)
    (let-values
        ([(data:name-USD data:$-USD)
          (let ([l (- (length data:name-USD) (length data:$-USD))])
            (cond
              [(positive? l)
               (values (list-tail data:name-USD l) data:$-USD)]
              [(negative? l)
               (values data:name-USD (list-tail data:$-USD (- l)))]
              [else
               (values data:name-USD data:$-USD)]))])
      (for/list : (Listof JSExpr)
                ([datum:name-USD (in-list data:name-USD)]
                 [datum:$-USD (in-list data:$-USD)])
        (match* (datum:name-USD datum:$-USD)
          [(`(,ts0 ,o0 ,h0 ,l0 ,c0 ,vol0 ,volCcy0 ,volCcyQuote0 ,confirm0 ,date0)
            `(,ts1 ,o1 ,h1 ,l1 ,c1 ,vol1 ,volCcy1 ,volCcyQuote1 ,confirm1 ,date1))
           #:when (and (jsexpr-number? ts0)
                       (jsexpr-number? ts1)
                       (jsexpr-number? o0)
                       (jsexpr-number? o1)
                       (jsexpr-number? h0)
                       (jsexpr-number? h1)
                       (jsexpr-number? l0)
                       (jsexpr-number? l1)
                       (jsexpr-number? c0)
                       (jsexpr-number? c1)
                       (jsexpr-number? vol0)
                       (jsexpr-number? vol1)
                       (jsexpr-number? volCcy0)
                       (jsexpr-number? volCcy1)
                       (jsexpr-number? volCcyQuote0)
                       (jsexpr-number? volCcyQuote1)
                       (boolean? confirm0)
                       (boolean? confirm1)
                       (string? date0)
                       (string? date1))
           (define ts          (assert (and (equal? ts0 ts1) ts0) jsexpr-number?))
           (define o           (assert (exact->inexact (/ o0 o1)) jsexpr-number?))
           (define h           (assert (exact->inexact (/ h0 h1)) jsexpr-number?))
           (define l           (assert (exact->inexact (/ l0 l1)) jsexpr-number?))
           (define c           (assert (exact->inexact (/ c0 c1)) jsexpr-number?))
           (define vol         0)
           (define volCcy      0)
           (define volCcyQuote 0)
           (define confirm     (and confirm0 confirm1))
           (define date        (assert (and (equal? date0 date1) date0) string?))
           `(,ts ,o ,h ,l ,c ,vol ,volCcy ,volCcyQuote ,confirm ,date)])))))

(: update-data (-> (Listof Any) (Listof JSExpr)))
(define update-data
  (λ (data)
    (for/list : (Listof JSExpr) ([datum (in-list data)])
      (match datum
        [`(,ts ,o ,h ,l ,c ,vol ,volCcy ,volCcyQuote ,confirm)
         #:when (and (string? ts)
                     (string? o)
                     (string? h)
                     (string? l)
                     (string? c)
                     (string? vol)
                     (string? volCcy)
                     (string? volCcyQuote)
                     (string? confirm))
         (let ([ts          (string->jsexpr-number ts)]
               [o           (string->jsexpr-number o)]
               [h           (string->jsexpr-number h)]
               [l           (string->jsexpr-number l)]
               [c           (string->jsexpr-number c)]
               [vol         (string->jsexpr-number vol)]
               [volCcy      (string->jsexpr-number volCcy)]
               [volCcyQuote (string->jsexpr-number volCcyQuote)]
               [confirm     (string=? confirm "1")]
               [date        (milliseconds->string (assert (string->number ts) real?) #t 'rfc2822)])
           `(,ts ,o ,h ,l ,c ,vol ,volCcy ,volCcyQuote ,confirm ,date))]))))


(: simple-download (-> String (HashTable Symbol JSExpr)))
(define simple-download
  (λ (url)
    (let loop : (HashTable Symbol JSExpr) ()
      (with-handlers ([exn:fail:network:errno? raise]
                      [exn:fail?
                       #;(λ (ex)
                           (and (exn:fail? ex)
                                (string-suffix? (exn-message ex) " failed (input terminated prematurely)")))
                       (λ (ex) (pretty-print ex) (loop))])
        (displayln (format "Downloading '~a' ..." url))
        (define in (get-pure-port (string->url url)))
        (define jsexpr (read-json in))
        (if (eof-object? jsexpr)
            (error 'simple-download "EOF")
            (assert jsexpr hash?))))))

(: download (-> String Symbol Symbol String Integer Integer Natural [#:init-data (Listof JSExpr)] JSExpr))
(define download
  (let ()
    (: make-step (-> Natural Natural))
    (define make-step (λ (limit) (* 1000 60 (bar) limit)))

    (λ (base-url name $ bar before after limit #:init-data [init-data '()])
      (define step (make-step limit))
      (define data
        (for/fold ([data : (Listof JSExpr) init-data])
                  ([current : Integer (in-range before after step)])
          (define make-url
            (let ([before current]
                  [after (if (< (- after current) step) after (+ current step))])
              (λ (name $)
                (format "~a?instId=~a-~a&bar=~a&before=~a&after=~a&limit=~a"
                        base-url name $ bar before after limit))))

          (define data:name-$
            (let ([jsexpr:name-$ (simple-download (make-url name $))])
              (case (hash-ref jsexpr:name-$ 'code)
                [("51001")
                 (case $
                   [(USD USDC USDT) (error 'download "Instrument ID does not exist")]
                   [else
                    (let* ([jsexpr:name-USD (simple-download (make-url name (USD)))]
                           [jsexpr:$-USD    (simple-download (make-url $ (USD)))]
                           [data:name-USD   (hash-ref jsexpr:name-USD 'data)]
                           [data:$-USD      (hash-ref jsexpr:$-USD 'data)]
                           [data:name-USD   (assert data:name-USD list?)]
                           [data:$-USD      (assert data:$-USD list?)]
                           [data:name-USD   (update-data data:name-USD)]
                           [data:$-USD      (update-data data:$-USD)]
                           [data:name-$     (data:name-USD->name-$ data:name-USD data:$-USD)])
                      data:name-$)])]
                [else
                 (let* ([data:name-$ (hash-ref jsexpr:name-$ 'data)]
                        [data:name-$ (assert data:name-$ list?)]
                        [data:name-$ (update-data data:name-$)])
                   data:name-$)])))

          (append data:name-$ data)))
      (define jsexpr (hasheq 'code 0 'data data 'msg ""))
      jsexpr)))

(: download-data
   (->* ((Pair Symbol Symbol)
         Integer
         Integer)
        (#:history? Boolean
         #:write-file? Boolean)
        JSExpr))
(define download-data
  (λ (inst-id before after #:history? [history? #t] #:write-file? [write-file? #t])
    (define name (car inst-id))
    (define $    (cdr inst-id))
    (define-values (base-url limit)
      (let-values
          ([(request limit)
            (if history?
                (values #"history-candles" 100)
                (values #"candles"         300))])
        (values (format "https://www.okx.com/api/v5/market/~a" request) limit)))
    (define timestamp (milliseconds->string after))
    (define path:name-$ (format "data/~a/~a-~a-trades-~a.json" (bar->string (bar)) name $ timestamp))
    (let-values
        ([(before #;after init-data)
          (cond
            [(file-exists? path:name-$)
             (displayln (format "Reading '~a' ..." path:name-$))
             (define jsexpr (call-with-input-file path:name-$ read-json))
             (if (eof-object? jsexpr)
                 (error 'download-data "EOF")
                 (let* ([jsexpr (assert jsexpr hash?)]
                        [data (hash-ref jsexpr 'data)]
                        [data (assert data list?)])
                   (if (<= (length data) 1)
                       (values before #;after '())
                       (let* ([datum (car data)]
                              [datum (assert datum list?)]
                              [data (if (list-ref datum 8) data (cdr data))]
                              [datum (car data)]
                              [datum (assert datum list?)]
                              [ts (car datum)]
                              [ts (assert ts natural?)]
                              [before ts])
                         (values before #;after data)))))]
            [else (values before #;after '())])])
      (define jsexpr (download base-url name $ (bar->string (bar)) before after limit #:init-data init-data))
      (when write-file?
        (define bstr (jsexpr->bytes jsexpr))
        (call-with-output-file path:name-$
          #:exists 'truncate/replace
          (ann
           (λ (out)
             (displayln (format "Writing '~a' ..." path:name-$))
             (write-bytes bstr out))
           (-> Output-Port Any))))
      jsexpr)))

(: download-daily-data
   (->* ((Pair Symbol Symbol)
         String)
        (#:history? Boolean
         #:write-file? Boolean)
        JSExpr))
(define (download-daily-data inst-id day #:history? [history? #t] #:write-file? [write-file? #t])
  (: before Integer)
  (define before (sub1 (string->milliseconds day)))
  (: after Integer)
  (define after
    (if (today? day)
        (assert (current-milliseconds) exact-integer?)
        (+ before #;1D=_ms 86400000)))
  (download-data inst-id before after #:history? history? #:write-file? write-file?))

(: download-daily-data*
   (->* ((Listof (Pair Symbol Symbol))
         (Listof String))
        (#:history? Boolean
         #:write-file? Boolean)
        (Listof JSExpr)))
(define (download-daily-data* inst-id* day* #:history? [history? #t] #:write-file? [write-file? #t])
  (for*/list : (Listof JSExpr)
             ([day : String (in-list day*)]
              [inst-id : (Pair Symbol Symbol) inst-id*])
    (download-daily-data inst-id day #:history? history? #:write-file? write-file?)))


(: cache (Mutable-HashTable
          (Immutable-Vector Natural (Pair Symbol Symbol) String)
          (HashTable Symbol JSExpr)))
(define cache (make-hash))

(: get-r* (-> (Pair Symbol Symbol) Natural Integer Integer (Listof Nonnegative-Real)))
(define get-r*
  (λ (inst-id bar before after)
    (define name (car inst-id))
    (define $ (cdr inst-id))
    (for/fold ([r* : (Listof Nonnegative-Real) '()])
              ([day (in-list (dates-between before after))])
      (define key (vector-immutable bar inst-id day))
      (check-key! key)
      (define jsexpr (hash-ref cache key))
      (let* ([data (hash-ref jsexpr 'data)]
             [data (assert data list?)])
        (for/fold ([r* : (Listof Nonnegative-Real) r*])
                  ([datum (in-list data)]
                   #:when (list? datum))
          #:break (let* ([ts (list-ref datum 0)]
                         [ts (assert ts natural?)])
                    (not (<= before ts after)))
          (let* ([o (list-ref datum 1)]
                 [o (assert o real?)]
                 [c (list-ref datum 4)]
                 [c (assert c real?)]
                 [r (/ c o)]
                 [r (assert r positive?)])
            (cons r r*)))))))

(: update-cache!
   (-> (Immutable-Vector Natural (Pair Symbol Symbol) String)
       [#:jsexpr (Option JSExpr)]
       [#:force? Boolean]
       Void))
(define update-cache!
  (λ (key #:jsexpr [jsexpr #f] #:force? [force? #f])
    (define inst-id (vector-ref key 1))
    (define name (car inst-id))
    (define $ (cdr inst-id))
    (define day (vector-ref key 2))
    (define path (format "data/~a/~a-~a-trades-~a.json" (bar->string (bar)) name $ day))
    (let* ([jsexpr
            (or jsexpr
                (begin
                  (when (and (file-exists? path) force?)
                    (displayln (format "Deleting '~a' ..." path))
                    (delete-file path))
                  (if (file-exists? path)
                      (begin
                        (displayln (format "Reading '~a' ..." path))
                        (call-with-input-file path read-json))
                      (download-daily-data inst-id day #:history? (not (today? day))))))]
           [jsexpr
            (if (eof-object? jsexpr)
                (error 'simple-download "EOF")
                (assert jsexpr hash?))]
           [data (hash-ref jsexpr 'data)]
           [data (assert data list?)]
           [data
            (if (null? data)
                '()
                (let* ([datum (car data)]
                       [datum (assert datum list?)]
                       [data (if (list-ref datum 8) data (cdr data))])
                  data))])
      (define step (* 1000 60 (bar)))
      (define ts0
        (if (null? data)
            0
            (let* ([datum (car data)]
                   [datum (assert datum list?)]
                   [ts (list-ref datum 0)]
                   [ts (assert ts natural?)])
              ts)))
      (define len
        (for/fold ([len : Natural 0])
                  ([datum (in-list data)]
                   [ts (in-range ts0 -inf.0 (- step))]
                   #:when (list? datum))
          #:break (not (eq? ts (list-ref datum 0)))
          (add1 len)))
      (if (= len (length data))
          (let ([jsexpr : (HashTable Symbol JSExpr) (hash-set jsexpr 'data data)])
            (hash-set! cache key jsexpr)
            (check-key! key))
          (update-cache! key #:force? #t)))))

(: check-key! (-> (Immutable-Vector Natural (Pair Symbol Symbol) String) Void))
(define (check-key! key)
  (cond
    [(hash-has-key? cache key)
     (define day (vector-ref key 2))
     (define limit (quotient (minutes-elapsed-since-midnight day) (bar)))
     (define jsexpr (hash-ref cache key))
     (define data (assert (hash-ref jsexpr 'data) list?))
     (define len (length data))
     (cond
       [(<= 0 (- limit len) 1) (void)]
       [(< len limit)
        (define inst-id (vector-ref key 1))
        (define jsexpr (download-daily-data inst-id day #:history? (not (today? day))))
        (update-cache! key #:jsexpr jsexpr)]
       [(> len limit)
        (displayln (format "Warning: ~a = length(r*) > limit = ~a!" len limit))
        (update-cache! key #:force? #t)])]
    [else (update-cache! key)]))


(: main (->* () ((Vectorof String)) Any))
(define main
  (λ ([argv (current-command-line-arguments)])
    (for ([i : Natural (in-list '(1 3 5 15 30 60 120 240 360 720 1440))])
      (parameterize ([bar i])
        (download-daily-data*
         '([ETH . BTC]
           [LTC . BTC]
           #;[OKB . BTC]
           #;[BNB . BTC]
           #;[BTC . USDC]
           #;[ETH . USDC]
           #;[LTC . USDC]
           #;[OKB . USDC]
           #;[BNB . USDC])
         (dates-between "2022-01-01" "2022-12-31"))))
    #t))
(module+ main (exit (main)))
