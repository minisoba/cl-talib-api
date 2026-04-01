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
   #:talib-type-error

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

   ;; lookback — overlap
   #:accbands-lookback
   #:bbands-lookback
   #:dema-lookback
   #:ema-lookback
   #:kama-lookback
   #:ma-lookback
   #:mama-lookback
   #:mavp-lookback
   #:midpoint-lookback
   #:midprice-lookback
   #:sar-lookback
   #:sar-ext-lookback
   #:sma-lookback
   #:t3-lookback
   #:tema-lookback
   #:trima-lookback
   #:wma-lookback

   ;; lookback — momentum
   #:adx-lookback
   #:adxr-lookback
   #:apo-lookback
   #:aroon-lookback
   #:aroon-osc-lookback
   #:bop-lookback
   #:cci-lookback
   #:cmo-lookback
   #:dx-lookback
   #:macd-lookback
   #:macd-ext-lookback
   #:macd-fix-lookback
   #:mfi-lookback
   #:minus-di-lookback
   #:minus-dm-lookback
   #:mom-lookback
   #:plus-di-lookback
   #:plus-dm-lookback
   #:ppo-lookback
   #:roc-lookback
   #:rocp-lookback
   #:rocr-lookback
   #:rocr100-lookback
   #:rsi-lookback
   #:stoch-lookback
   #:stochf-lookback
   #:stoch-rsi-lookback
   #:trix-lookback
   #:ult-osc-lookback
   #:willr-lookback

   ;; lookback — volume
   #:ad-lookback
   #:ad-osc-lookback
   #:obv-lookback

   ;; lookback — volatility
   #:atr-lookback
   #:natr-lookback
   #:trange-lookback

   ;; parquet
   #:save-to-parquet
   ))

(pushnew (merge-pathnames "lib/"
                          (asdf:system-source-directory :cl-talib-api))
         cffi:*foreign-library-directories*
         :test #'equal)

(cffi:define-foreign-library libta-lib
  (:darwin  "libta-lib.dylib")
  (:unix    "libta-lib.so")
  (:windows "libta-lib.dll")
  (t "libta-lib"))

(cffi:use-foreign-library libta-lib)
