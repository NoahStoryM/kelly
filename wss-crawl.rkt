#lang racket

(require json)
(require net/rfc6455 net/url)
(require web-server/http/request-structs)
(require base64 crypto crypto/libcrypto)
(crypto-factories libcrypto-factory)


(define okx (call-with-input-file "okx-api.json" read-json))
(define api-key (hash-ref okx '|API key|))
(define passphrase (hash-ref okx '|Passphrase|))
(define secret-key (hash-ref okx '|Secret key|))

#;(define login
    (λ ()
      (define timestamp (number->string (current-seconds)))
      (define method #"GET")
      (define url (string->url "wss://ws.okx.com:8443/ws/v5/private?brokerId=9999"))
      (define uri #"/users/self/verify")
      (define body #"")
      (define sign
        (bytes->string/utf-8
         (base64-encode
          #:pad? #t
          (hmac
           'sha256
           (string->bytes/utf-8 secret-key)
           (bytes-append
            (string->bytes/utf-8 timestamp)
            method uri body)))))

      (define json:login-request
        (jsexpr->bytes
         (hasheq
          'op "login"
          'args (list
                 (hasheq
                  'apiKey api-key
                  'passphrase passphrase
                  'timestamp timestamp
                  'sign sign)))))

      (with-handlers ([exn:fail:network:errno? (λ (ex) (sleep 1) (login))])
        (sleep 1)
        (displayln #"Connecting...")
        (define ws (ws-connect url))

        (displayln #"Try Send login request.")
        (ws-send! ws json:login-request)

        (displayln #"Try Receive response.")
        (pretty-print (bytes->jsexpr (ws-recv ws #:payload-type 'binary)))

        (displayln #"Try Close connection.")
        (ws-close! ws))))

(define candle-chart
  (λ (args)
    (define url (string->url "wss://ws.okx.com:8443/ws/v5/public?brokerId=9999"))
    (define json:candle-chart-request
      (jsexpr->bytes
       (hasheq
        'op "subscribe"
        'args args)))

    (with-handlers ([exn:fail:network:errno? (λ (ex) (sleep 1) (candle-chart args))])
      (displayln #"Connecting...")
      (define ws (ws-connect url))

      (displayln #"Try Send candle-chart request.")
      (ws-send! ws json:candle-chart-request)

      (displayln #"Try Receive response.")
      (pretty-print (bytes->jsexpr (ws-recv ws #:payload-type 'binary)))

      (displayln #"Try Close connection.")
      (ws-close! ws))))

(candle-chart
 (list
  (hasheq
   'channel "tickers"
   'instId "ETH-BTC")))
