(in-package :cl-user)

(defpackage :cl-talib-api
  (:use :cl :alexandria)
  (:nicknames :talib)
  (:export
   #:time-series-1
   #:make-time-series-1
   #:time-series-2
   #:make-time-series-2
   #:time-series-3
   #:make-time-series-3
   #:time-series-4
   #:make-time-series-4
   #:talib-error

   #:stock-prices
   #:open-prices-of
   #:high-prices-of
   #:low-prices-of
   #:close-prices-of
   #:volumes-of
   #:make-stock-prices
   #:destroy

   #:make-foreign-object
   #:free-foreign-object

   ;; overlap
   #:accbands
   #:bbands
   #:dema
   #:ema
   #:kama
   #:ma
   #:mama
   #:mavp
   #:midpoint
   #:midprice
   #:sar
   #:sar-ext
   #:sma
   #:t3
   #:tema
   #:trima
   #:wma

   ;; momentum
   #:adx
   #:adxr
   #:apo
   #:aroon
   #:aroon-osc
   #:bop
   #:cci
   #:cmo
   #:dx
   #:macd
   #:macd-ext
   #:macd-fix
   #:mfi
   #:minus-di
   #:minus-dm
   #:mom
   #:plus-di
   #:plus-dm
   #:ppo
   #:roc
   #:rocp
   #:rocr
   #:rocr100
   #:rsi
   #:stoch
   #:stochf
   #:stoch-rsi
   #:trix
   #:ult-osc
   #:willr

   ;; volume
   #:ad
   #:ad-osc
   #:obv

   ;; volatility
   #:atr
   #:natr
   #:trange
   ))

(cffi:define-foreign-library libta-lib
  (:darwin  "libta-lib.dylib")
  (:unix    "libta-lib.so")
  (:windows "libta-lib.dll")
  (t "libta-lib"))

(cffi:use-foreign-library libta-lib)
