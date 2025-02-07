(in-package :cl-talib-api)

(cffi:defcenum :ret-code
  :success
  :lib-not-initialize
  :bad-param
  :alloc-err
  :group-not-found
  :func-not-found
  :invalid-handle
  :invalid-param-holder
  :invalid-param-holder-type
  :invalid-param-function
  :input-not-all-initialize
  :output-not-all-initialize
  :out-of-range-start-index
  :out-of-range-end-index
  :invalid-list-type
  :bad-object
  :not-supported
  (:internal-error 5000)
  (:unknown-error 65535)) ;; (parse-integer "FFFF" :radix 16)

(cffi:defcenum :ma-type
  :sma
  :ema
  :wma
  :dema
  :tema
  :trima
  :kama
  :mama
  :t3)

;; overlap

(cffi:defcfun ("TA_ACCBANDS" %accbands) :ret-code
  (start-idx            :int)
  (end-idx              :int)
  (in-high              (:pointer :double))
  (in-low               (:pointer :double))
  (in-close             (:pointer :double))
  (opt-in-time-period   :int)
  (out-beg-idx          (:pointer :int))
  (out-nb-element       (:pointer :int))
  (out-real-upper-band  (:pointer :double))
  (out-real-middle-band (:pointer :double))
  (out-real-lower-band  (:pointer :double)))

(cffi:defcfun ("TA_BBANDS" %bbands) :ret-code
  (start-idx            :int)
  (end-idx              :int)
  (in-real              (:pointer :double))
  (opt-in-time-period   :int)
  (opt-in-nb-dev-up     :double)
  (opt-in-nb-dev-down   :double)
  (opt-in-ma-type       :ma-type)
  (out-beg-idx          (:pointer :int))
  (out-nb-element       (:pointer :int))
  (out-real-upper-band  (:pointer :double))
  (out-real-middle-band (:pointer :double))
  (out-real-lower-band  (:pointer :double)))

(cffi:defcfun ("TA_DEMA" %dema) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-real            (:pointer :double))
  (opt-in-time-period :int)
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-real           (:pointer :double)))

(cffi:defcfun ("TA_EMA" %ema) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-real            (:pointer :double))
  (opt-in-time-period :int)
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-real           (:pointer :double)))

(cffi:defcfun ("TA_KAMA" %kama) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-real            (:pointer :double))
  (opt-in-time-period :int)
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-real           (:pointer :double)))

(cffi:defcfun ("TA_MA" %ma) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-real            (:pointer :double))
  (opt-in-time-period :int)
  (opt-in-ma-type     :ma-type)
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-real           (:pointer :double)))

(cffi:defcfun ("TA_MAMA" %mama) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-real            (:pointer :double))
  (opt-in-fast-limit  :double)
  (opt-in-slow-limit  :double)
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-mama           (:pointer :double))
  (out-fama           (:pointer :double)))

(cffi:defcfun ("TA_MAVP" %mavp) :ret-code
  (start-idx         :int)
  (end-idx           :int)
  (in-real           (:pointer :double))
  (in-periods        (:pointer :double))
  (opt-in-min-period :int)
  (opt-in-max-period :int)
  (opt-in-ma-type    :ma-type)
  (out-beg-idx       (:pointer :int))
  (out-nb-element    (:pointer :int))
  (out-real          (:pointer :double)))

(cffi:defcfun ("TA_MIDPOINT" %midpoint) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-real            (:pointer :double))
  (opt-in-time-period :int)
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-real           (:pointer :double)))

(cffi:defcfun ("TA_MIDPRICE" %midprice) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-high            (:pointer :double))
  (in-low             (:pointer :double))
  (opt-in-time-period :int)
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-real           (:pointer :double)))

(cffi:defcfun ("TA_SAR" %sar) :ret-code
  (start-idx           :int)
  (end-idx             :int)
  (in-high             (:pointer :double))
  (in-low              (:pointer :double))
  (opt-in-acceleration :double)
  (opt-in-maximum      :double)
  (out-beg-idx         (:pointer :int))
  (out-nb-element      (:pointer :int))
  (out-real            (:pointer :double)))

(cffi:defcfun ("TA_SAREXT" %sar-ext) :ret-code
  (start-idx                      :int)
  (end-idx                        :int)
  (in-high                        (:pointer :double))
  (in-low                         (:pointer :double))
  (opt-in-start-value             :double)
  (opt-in-offset-on-reverse       :double)
  (opt-in-acceleration-init-long  :double)
  (opt-in-acceleration-long       :double)
  (opt-in-acceleration-max-long   :double)
  (opt-in-acceleration-init-short :double)
  (opt-in-acceleration-short      :double)
  (opt-in-acceleration-max-short  :double)
  (out-beg-idx                    (:pointer :int))
  (out-nb-element                 (:pointer :int))
  (out-real                       (:pointer :double)))

(cffi:defcfun ("TA_SMA" %sma) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-real            (:pointer :double))
  (opt-in-time-period :int)
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-real           (:pointer :double)))

(cffi:defcfun ("TA_T3" %t3) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-real            (:pointer :double))
  (opt-in-time-period :int)
  (opt-in-v-factor    :double)
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-real           (:pointer :double)))

(cffi:defcfun ("TA_TEMA" %tema) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-real            (:pointer :double))
  (opt-in-time-period :int)
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-real           (:pointer :double)))

(cffi:defcfun ("TA_TRIMA" %trima) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-real            (:pointer :double))
  (opt-in-time-period :int)
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-real           (:pointer :double)))

(cffi:defcfun ("TA_WMA" %wma) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-real            (:pointer :double))
  (opt-in-time-period :int)
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-real           (:pointer :double)))

;; momentum

(cffi:defcfun ("TA_ADX" %adx) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-high            (:pointer :double))
  (in-low             (:pointer :double))
  (in-close           (:pointer :double))
  (opt-in-time-period :int)
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-real           (:pointer :double)))

(cffi:defcfun ("TA_ADXR" %adxr) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-high            (:pointer :double))
  (in-low             (:pointer :double))
  (in-close           (:pointer :double))
  (opt-in-time-period :int)
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-real           (:pointer :double)))

(cffi:defcfun ("TA_APO" %apo) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-real            (:pointer :double))
  (opt-in-fast-period :int)
  (opt-in-slow-period :int)
  (opt-in-ma-type     :ma-type)
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-real           (:pointer :double)))

(cffi:defcfun ("TA_AROON" %aroon) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-high            (:pointer :double))
  (in-low             (:pointer :double))
  (opt-in-time-period :int)
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-up             (:pointer :double))
  (out-down           (:pointer :double)))

(cffi:defcfun ("TA_AROONOSC" %aroon-osc) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-high            (:pointer :double))
  (in-low             (:pointer :double))
  (opt-in-time-period :int)
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-real           (:pointer :double)))

(cffi:defcfun ("TA_BOP" %bop) :ret-code
  (start-idx      :int)
  (end-idx        :int)
  (in-open        (:pointer :double))
  (in-high        (:pointer :double))
  (in-low         (:pointer :double))
  (in-close       (:pointer :double))
  (out-beg-idx    (:pointer :int))
  (out-nb-element (:pointer :int))
  (out-real       (:pointer :double)))

(cffi:defcfun ("TA_CCI" %cci) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-high            (:pointer :double))
  (in-low             (:pointer :double))
  (in-close           (:pointer :double))
  (opt-in-time-period :int)
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-real           (:pointer :double)))

(cffi:defcfun ("TA_CMO" %cmo) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-real            (:pointer :double))
  (opt-in-time-period :int)
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-real           (:pointer :double)))

(cffi:defcfun ("TA_DX" %dx) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-high            (:pointer :double))
  (in-low             (:pointer :double))
  (in-close           (:pointer :double))
  (opt-in-time-period :int)
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-integer        (:pointer :double)))

(cffi:defcfun ("TA_MACD" %macd) :ret-code
  (start-idx            :int)
  (end-idx              :int)
  (in-real              (:pointer :double))
  (opt-in-fast-period   :int)
  (opt-in-slow-period   :int)
  (opt-in-signal-period :int)
  (out-beg-idx          (:pointer :int))
  (out-nb-element       (:pointer :int))
  (out-macd             (:pointer :double))
  (out-macd-signal      (:pointer :double))
  (out-macd-hist        (:pointer :double)))

(cffi:defcfun ("TA_MACDEXT" %macd-ext) :ret-code
  (start-idx             :int)
  (end-idx               :int)
  (in-real               (:pointer :double))
  (opt-in-fast-period    :int)
  (opt-in-fast-ma-type   :ma-type)
  (opt-in-slow-period    :int)
  (opt-in-slow-ma-type   :ma-type)
  (opt-in-signal-period  :int)
  (opt-in-signal-ma-type :ma-type)
  (out-beg-idx           (:pointer :int))
  (out-nb-element        (:pointer :int))
  (out-macd              (:pointer :double))
  (out-macd-signal       (:pointer :double))
  (out-macd-hist         (:pointer :double)))

(cffi:defcfun ("TA_MACDFIX" %macd-fix) :ret-code
  (start-idx            :int)
  (end-idx              :int)
  (in-real              (:pointer :double))
  (opt-in-signal-period :int)
  (out-beg-idx          (:pointer :int))
  (out-nb-element       (:pointer :int))
  (out-macd             (:pointer :double))
  (out-macd-signal      (:pointer :double))
  (out-macd-hist        (:pointer :double)))

(cffi:defcfun ("TA_MFI" %mfi) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-high            (:pointer :double))
  (in-low             (:pointer :double))
  (in-close           (:pointer :double))
  (in-volume          (:pointer :double))
  (opt-in-time-period :int)
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-real           (:pointer :double)))

(cffi:defcfun ("TA_MINUS_DI" %minus-di) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-high            (:pointer :double))
  (in-low             (:pointer :double))
  (in-close           (:pointer :double))
  (opt-in-time-period :int)
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-real           (:pointer :double)))

(cffi:defcfun ("TA_MINUS_DM" %minus-dm) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-high            (:pointer :double))
  (in-low             (:pointer :double))
  (opt-in-time-period :int)
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-real           (:pointer :double)))

(cffi:defcfun ("TA_MOM" %mom) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-real            (:pointer :double))
  (opt-in-time-period :int)
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-real           (:pointer :double)))

(cffi:defcfun ("TA_PLUS_DI" %plus-di) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-high            (:pointer :double))
  (in-low             (:pointer :double))
  (in-close           (:pointer :double))
  (opt-in-time-period :int)
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-real           (:pointer :double)))

(cffi:defcfun ("TA_PLUS_DM" %plus-dm) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-high            (:pointer :double))
  (in-low             (:pointer :double))
  (opt-in-time-period :int)
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-real           (:pointer :double)))

(cffi:defcfun ("TA_PPO" %ppo) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-real            (:pointer :double))
  (opt-in-fast-period :int)
  (opt-in-slow-period :int)
  (opt-in-ma-type     :ma-type)
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-real           (:pointer :double)))

(cffi:defcfun ("TA_ROC" %roc) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-real            (:pointer :double))
  (opt-in-time-period :int)
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-real           (:pointer :double)))

(cffi:defcfun ("TA_ROCP" %rocp) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-real            (:pointer :double))
  (opt-in-time-period :int)
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-real           (:pointer :double)))

(cffi:defcfun ("TA_ROCR" %rocr) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-real            (:pointer :double))
  (opt-in-time-period :int)
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-real           (:pointer :double)))

(cffi:defcfun ("TA_ROCR100" %rocr100) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-real            (:pointer :double))
  (opt-in-time-period :int)
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-real           (:pointer :double)))

(cffi:defcfun ("TA_RSI" %rsi) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-real            (:pointer :double))
  (opt-in-time-period :int)
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-real           (:pointer :double)))

(cffi:defcfun ("TA_STOCH" %stoch) :ret-code
  (start-idx             :int)
  (end-idx               :int)
  (in-high               (:pointer :double))
  (in-low                (:pointer :double))
  (in-close              (:pointer :double))
  (opt-in-fast-k-period  :int)
  (opt-in-slow-k-period  :int)
  (opt-in-slow-k-ma-type :ma-type)
  (opt-in-slow-d-period  :int)
  (opt-in-slow-d-ma-type :ma-type)
  (out-beg-idx           (:pointer :int))
  (out-nb-element        (:pointer :int))
  (out-slow-k            (:pointer :double))
  (out-slow-d            (:pointer :double)))

(cffi:defcfun ("TA_STOCHF" %stochf) :ret-code
  (start-idx             :int)
  (end-idx               :int)
  (in-high               (:pointer :double))
  (in-low                (:pointer :double))
  (in-close              (:pointer :double))
  (opt-in-fast-k-period  :int)
  (opt-in-fast-d-period  :int)
  (opt-in-fast-d-ma-type :ma-type)
  (out-beg-idx           (:pointer :int))
  (out-nb-element        (:pointer :int))
  (out-fast-k            (:pointer :double))
  (out-fast-d            (:pointer :double)))

(cffi:defcfun ("TA_STOCHRSI" %stoch-rsi) :ret-code
  (start-idx             :int)
  (end-idx               :int)
  (in-real               (:pointer :double))
  (opt-in-time-period    :int)
  (opt-in-fast-k-period  :int)
  (opt-in-fast-d-period  :int)
  (opt-in-fast-d-ma-type :ma-type)
  (out-beg-idx           (:pointer :int))
  (out-nb-element        (:pointer :int))
  (out-fast-k            (:pointer :double))
  (out-fast-d            (:pointer :double)))

(cffi:defcfun ("TA_TRIX" %trix) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-real            (:pointer :double))
  (opt-in-time-period :int)
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-real           (:pointer :double)))

(cffi:defcfun ("TA_ULTOSC" %ult-osc) :ret-code
  (start-idx           :int)
  (end-idx             :int)
  (in-high             (:pointer :double))
  (in-low              (:pointer :double))
  (in-close            (:pointer :double))
  (opt-in-time-period1 :int)
  (opt-in-time-period2 :int)
  (opt-in-time-period3 :int)
  (out-beg-idx         (:pointer :int))
  (out-nb-element      (:pointer :int))
  (out-real            (:pointer :double)))

(cffi:defcfun ("TA_WILLR" %willr) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-high            (:pointer :double))
  (in-low             (:pointer :double))
  (in-close           (:pointer :double))
  (opt-in-time-period :int)
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-real           (:pointer :double)))

;; volume

(cffi:defcfun ("TA_AD" %ad) :ret-code
  (start-idx      :int)
  (end-idx        :int)
  (in-high        (:pointer :double))
  (in-low         (:pointer :double))
  (in-close       (:pointer :double))
  (in-volume      (:pointer :double))
  (out-beg-idx    (:pointer :int))
  (out-nb-element (:pointer :int))
  (out-real       (:pointer :double)))

(cffi:defcfun ("TA_ADOSC" %ad-osc) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-high            (:pointer :double))
  (in-low             (:pointer :double))
  (in-close           (:pointer :double))
  (in-volume          (:pointer :double))
  (opt-in-fast-period :int)
  (opt-in-slow-period :int)
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-real           (:pointer :double)))

(cffi:defcfun ("TA_OBV" %obv) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-real            (:pointer :double))
  (in-volume          (:pointer :double))
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-real           (:pointer :double)))

;; volatility

(cffi:defcfun ("TA_ATR" %atr) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-high            (:pointer :double))
  (in-low             (:pointer :double))
  (in-close           (:pointer :double))
  (opt-in-time-period :int)
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-real           (:pointer :double)))

(cffi:defcfun ("TA_NATR" %natr) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-high            (:pointer :double))
  (in-low             (:pointer :double))
  (in-close           (:pointer :double))
  (opt-in-time-period :int)
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-real           (:pointer :double)))

(cffi:defcfun ("TA_TRANGE" %trange) :ret-code
  (start-idx          :int)
  (end-idx            :int)
  (in-high            (:pointer :double))
  (in-low             (:pointer :double))
  (in-close           (:pointer :double))
  (out-beg-idx        (:pointer :int))
  (out-nb-element     (:pointer :int))
  (out-real           (:pointer :double)))
