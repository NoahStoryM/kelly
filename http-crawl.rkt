#lang racket

(require json)
(require net/http-client net/url)
(require web-server/http/request-structs)
(require base64 crypto crypto/libcrypto)
(crypto-factories libcrypto-factory)
(require racket/date)
(date-display-format 'iso-8601)


(define okx (call-with-input-file "okx-api.json" read-json))
(define api-key (string->bytes/utf-8 (hash-ref okx '|API key|)))
(define passphrase (string->bytes/utf-8 (hash-ref okx '|Passphrase|)))
(define secret-key (string->bytes/utf-8 (hash-ref okx '|Secret key|)))


(define candle-chart
  (λ (body)
    (let loop ([times 1])
      (define timestamp (string->bytes/utf-8 (date->string (current-date) #t)))
      (define method #"GET")
      (define host #"okx.com")
      (define uri #"/api/v5/market/history-candles?instId=ETH-BTC")

      (define sign (base64-encode #:pad? #t (hmac 'sha256 secret-key (bytes-append timestamp method uri body))))

      (with-handlers ([exn:fail:network:errno? (λ (ex) (sleep 1) (loop (add1 times)))])
        (displayln (format "Connecting ~a times..." times))
        (define-values (status headers in)
          (http-sendrecv
           host uri
           #:method method
           #:headers
           (list
            #;#"x-simulated-trading: 1"
            #;#"accept: application/json"
            #;#"Content-Type: application/json"
            #;(bytes-append #"OK-ACCESS-KEY: " api-key)
            #;(bytes-append #"OK-ACCESS-SIGN: " sign)
            #;(bytes-append #"OK-ACCESS-PASSPHRASE: " passphrase)
            #;(bytes-append #"OK-ACCESS-TIMESTAMP: " timestamp)
            #;(bytes-append #"expTime: " (string->bytes/utf-8 (number->string (current-milliseconds)))))
           #:data body))

        (displayln #"Try Receive response.")
        (pretty-print (list status headers (port->bytes in)))))))

(candle-chart #"" #;(jsexpr->bytes (hasheq 'instId "ETH-BTC")))
