#lang typed/racket/base

(require typed/net/url typed/web-server/http #;typed/web-server/web-server)

(require typed/racket/unsafe)
(unsafe-require/typed/provide net/rfc6455
  [ws-url?  (-> Any Boolean : #:+ URL)]
  [wss-url? (-> Any Boolean : #:+ URL)])

(require/typed/provide net/rfc6455
  [#:opaque WS-Connection ws-conn?]

  [ws-conn-supports-fragmentation?  (-> WS-Connection Boolean)]
  [ws-conn-supports-payload-type?   (-> WS-Connection Symbol Boolean)]
  [ws-conn-signals-status-on-close? (-> WS-Connection Boolean)]
  [ws-conn-closed?      (-> WS-Connection Boolean)]
  [ws-conn-close-status (-> WS-Connection (Option Number))]
  [ws-conn-close-reason (-> WS-Connection (Option String))]
  [ws-connect (-> URL [#:headers (Listof Header)] [#:protocol (U 'rfc6455 'hybi00)] WS-Connection)]
  #;[ws-serve  ]
  #;[ws-serve* ]
  [ws-send! (-> WS-Connection
                (U String Bytes Input-Port)
                [#:final-fragment? Boolean]
                [#:payload-type (U 'continuation 'text 'binary)]
                [#:flush? Boolean]
                Void)]
  [ws-recv        (-> WS-Connection [#:payload-type (U 'auto 'text 'binary)] (U EOF String Bytes))]
  [ws-recv-evt    (-> WS-Connection [#:payload-type (U 'auto 'text 'binary)] (Evtof (U EOF String Bytes)))]
  [ws-recv-stream (-> WS-Connection Input-Port)]
  [ws-close! (-> WS-Connection [#:status Integer] [#:reason String] Void)]
  [rfc6455-stream-buffer-size (Parameter Integer)]
  [hybi00-framing-mode (Parameter (U 'new 'old))]
  [ws-idle-timeout (Parameter Number)])
