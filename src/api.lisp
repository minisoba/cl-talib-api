(in-package :cl-talib-api)

;; overlap

(defmethod accbands ((series time-series-3) start-idx end-idx &key (time-period 10))
  (create-ta-series-3 %accbands series start-idx end-idx :in-args `(,time-period)))

(defmethod bbands ((series time-series-1) start-idx end-idx
                   &key (time-period 5) (nb-dev-up 2.0) (nb-dev-down 2.0) (ma-type :sma))
  (setf nb-dev-up (ensure-double-float nb-dev-up 'nb-dev-up))
  (setf nb-dev-down (ensure-double-float nb-dev-down 'nb-dev-down))
  (create-ta-series-3
   %bbands series start-idx end-idx
   :in-args `(,time-period ,nb-dev-up ,nb-dev-down ,ma-type)))

(defmethod dema ((series time-series-1) start-idx end-idx &key (time-period 10))
  (create-ta-series-1 %dema series start-idx end-idx :in-args `(,time-period)))

(defmethod ema ((series time-series-1) start-idx end-idx &key (time-period 10))
  (create-ta-series-1 %ema series start-idx end-idx :in-args `(,time-period)))

(defmethod kama ((series time-series-1) start-idx end-idx &key (time-period 10))
  (create-ta-series-1 %kama series start-idx end-idx :in-args `(,time-period)))

(defmethod ma ((series time-series-1) start-idx end-idx
               &key (time-period 10) (ma-type :sma))
  (create-ta-series-1 %ma series start-idx end-idx :in-args `(,time-period ,ma-type)))

(defmethod mama ((series time-series-1) start-idx end-idx
                 &key (fast-limit 0.01) (slow-limit 0.01))
  (setf fast-limit (ensure-double-float fast-limit 'fast-limit))
  (setf slow-limit (ensure-double-float slow-limit 'slow-limit))
  (create-ta-series-2
   %mama series start-idx end-idx
   :in-args `(,fast-limit ,slow-limit)))

(defmethod mavp ((series time-series-2) start-idx end-idx
                 &key (min-period 2) (max-period 10) (ma-type :sma))
  (create-ta-series-1
   %mavp series start-idx end-idx
   :in-args `(,min-period ,max-period ,ma-type)))

(defmethod midpoint ((series time-series-1) start-idx end-idx &key (time-period 2))
  (create-ta-series-1 %midpoint series start-idx end-idx :in-args `(,time-period)))

(defmethod midprice ((series time-series-2) start-idx end-idx &key (time-period 2))
  (create-ta-series-1 %midprice series start-idx end-idx :in-args `(,time-period)))

(defmethod sar ((series time-series-2) start-idx end-idx &key (acceleration 0.02) (maximum 0.2))
  (setf acceleration (ensure-double-float acceleration 'acceleration))
  (setf maximum (ensure-double-float maximum 'maximum))
  (create-ta-series-1 %sar series start-idx end-idx :in-args `(,acceleration ,maximum)))

(defmethod sar-ext ((series time-series-2) start-idx end-idx
                    &key (start-value 0.0) (offset-on-reverse 0.0)
                         (acceleration-init-long 0.02) (acceleration-long 0.02) (acceleration-max-long 0.2)
                         (acceleration-init-short 0.02) (acceleration-short 0.02) (acceleration-max-short 0.2))
  (setf start-value (ensure-double-float start-value 'start-value))
  (setf offset-on-reverse (ensure-double-float offset-on-reverse 'offset-on-reverse))
  (setf acceleration-init-long (ensure-double-float acceleration-init-long 'acceleration-init-long))
  (setf acceleration-long (ensure-double-float acceleration-long 'acceleration-long))
  (setf acceleration-max-long (ensure-double-float acceleration-max-long 'acceleration-max-long))
  (setf acceleration-init-short (ensure-double-float acceleration-init-short 'acceleration-init-short))
  (setf acceleration-short (ensure-double-float acceleration-short 'acceleration-short))
  (setf acceleration-max-short (ensure-double-float acceleration-max-short 'acceleration-max-short))
  (create-ta-series-1
   %sar-ext series start-idx end-idx
   :in-args `(,start-value ,offset-on-reverse
              ,acceleration-init-long ,acceleration-long ,acceleration-max-long
              ,acceleration-init-short ,acceleration-short ,acceleration-max-short)))

(defmethod sma ((series time-series-1) start-idx end-idx &key (time-period 10))
  (create-ta-series-1 %sma series start-idx end-idx :in-args `(,time-period)))

(defmethod t3 ((series time-series-1) start-idx end-idx &key (time-period 10) (v-factor 0.7))
  (setf v-factor (ensure-double-float v-factor 'v-factor))
  (create-ta-series-1 %t3 series start-idx end-idx :in-args `(,time-period ,v-factor)))

(defmethod tema ((series time-series-1) start-idx end-idx &key (time-period 10))
  (create-ta-series-1 %tema series start-idx end-idx :in-args `(,time-period)))

(defmethod trima ((series time-series-1) start-idx end-idx &key (time-period 10))
  (create-ta-series-1 %trima series start-idx end-idx :in-args `(,time-period)))

(defmethod wma ((series time-series-1) start-idx end-idx &key (time-period 10))
  (create-ta-series-1 %wma series start-idx end-idx :in-args `(,time-period)))

;; momentum

(defmethod adx ((series time-series-3) start-idx end-idx &key (time-period 14))
  (create-ta-series-1 %adx series start-idx end-idx :in-args `(,time-period)))

(defmethod adxr ((series time-series-3) start-idx end-idx &key (time-period 14))
  (create-ta-series-1 %adxr series start-idx end-idx :in-args `(,time-period)))

(defmethod apo ((series time-series-1) start-idx end-idx &key (fast-period 12) (slow-period 26) (ma-type :sma))
  (create-ta-series-1 %apo series start-idx end-idx :in-args `(,fast-period ,slow-period ,ma-type)))

(defmethod aroon ((series time-series-2) start-idx end-idx &key (time-period 14))
  (create-ta-series-2 %aroon series start-idx end-idx :in-args `(,time-period)))

(defmethod aroon-osc ((series time-series-2) start-idx end-idx &key (time-period 14))
  (create-ta-series-1 %aroon-osc series start-idx end-idx :in-args `(,time-period)))

(defmethod bop ((series time-series-4) start-idx end-idx)
  (create-ta-series-1 %bop series start-idx end-idx))

(defmethod cci ((series time-series-3) start-idx end-idx &key (time-period 14))
  (create-ta-series-1 %cci series start-idx end-idx :in-args `(,time-period)))

(defmethod cmo ((series time-series-1) start-idx end-idx &key (time-period 14))
  (create-ta-series-1 %cmo series start-idx end-idx :in-args `(,time-period)))

(defmethod dx ((series time-series-3) start-idx end-idx &key (time-period 14))
  (create-ta-series-1 %dx series start-idx end-idx :in-args `(,time-period)))

(defmethod macd ((series time-series-1) start-idx end-idx
                 &key (fast-period 12) (slow-period 26) (signal-period 9))
  (create-ta-series-3
   %macd series start-idx end-idx :in-args `(,fast-period ,slow-period ,signal-period)))

(defmethod macd-ext ((series time-series-1) start-idx end-idx
                 &key (fast-period 12) (fast-ma-type :sma)
                      (slow-period 26) (slow-ma-type :sma)
                      (signal-period 9) (signal-ma-type :sma))
  (create-ta-series-3
   %macd-ext series start-idx end-idx
   :in-args `(,fast-period ,fast-ma-type ,slow-period ,slow-ma-type ,signal-period ,signal-ma-type)))

(defmethod macd-fix ((series time-series-1) start-idx end-idx &key (signal-period 9))
  (create-ta-series-3 %macd-fix series start-idx end-idx :in-args `(,signal-period)))

(defmethod mfi ((series time-series-4) start-idx end-idx &key (time-period 14))
  (create-ta-series-1 %mfi series start-idx end-idx :in-args `(,time-period)))

(defmethod minus-di ((series time-series-3) start-idx end-idx &key (time-period 14))
  (create-ta-series-1 %minus-di series start-idx end-idx :in-args `(,time-period)))

(defmethod minus-dm ((series time-series-2) start-idx end-idx &key (time-period 14))
  (create-ta-series-1 %minus-dm series start-idx end-idx :in-args `(,time-period)))

(defmethod mom ((series time-series-1) start-idx end-idx &key (time-period 10))
  (create-ta-series-1 %mom series start-idx end-idx :in-args `(,time-period)))

(defmethod plus-di ((series time-series-3) start-idx end-idx &key (time-period 14))
  (create-ta-series-1 %plus-di series start-idx end-idx :in-args `(,time-period)))

(defmethod plus-dm ((series time-series-2) start-idx end-idx &key (time-period 14))
  (create-ta-series-1 %plus-dm series start-idx end-idx :in-args `(,time-period)))

(defmethod ppo ((series time-series-1) start-idx end-idx &key (fast-period 12) (slow-period 26) (ma-type :sma))
  (create-ta-series-1 %ppo series start-idx end-idx :in-args `(,fast-period ,slow-period ,ma-type)))

(defmethod roc ((series time-series-1) start-idx end-idx &key (time-period 10))
  (create-ta-series-1 %roc series start-idx end-idx :in-args `(,time-period)))

(defmethod rocp ((series time-series-1) start-idx end-idx &key (time-period 10))
  (create-ta-series-1 %rocp series start-idx end-idx :in-args `(,time-period)))

(defmethod rocr ((series time-series-1) start-idx end-idx &key (time-period 10))
  (create-ta-series-1 %rocr series start-idx end-idx :in-args `(,time-period)))

(defmethod rocr100 ((series time-series-1) start-idx end-idx &key (time-period 10))
  (create-ta-series-1 %rocr100 series start-idx end-idx :in-args `(,time-period)))

(defmethod rsi ((series time-series-1) start-idx end-idx &key (time-period 14))
  (create-ta-series-1 %rsi series start-idx end-idx :in-args `(,time-period)))

(defmethod stoch ((series time-series-3) start-idx end-idx
                   &key (fast-k-period 5)
                        (slow-k-period 3) (slow-k-ma-type :sma)
                        (slow-d-period 3) (slow-d-ma-type :sma))
  (create-ta-series-2
   %stoch series start-idx end-idx
   :in-args `(,fast-k-period ,slow-k-period ,slow-k-ma-type ,slow-d-period ,slow-d-ma-type)))

(defmethod stochf ((series time-series-3) start-idx end-idx
                   &key (fast-k-period 5) (fast-d-period 3) (fast-d-ma-type :sma))
  (create-ta-series-2
   %stochf series start-idx end-idx :in-args `(,fast-k-period ,fast-d-period ,fast-d-ma-type)))

(defmethod stoch-rsi ((series time-series-1) start-idx end-idx
                       &key (time-period 14) (fast-k-period 5) (fast-d-period 3) (fast-d-ma-type :sma))
  (create-ta-series-2
   %stoch-rsi series start-idx end-idx
   :in-args `(,time-period ,fast-k-period ,fast-d-period ,fast-d-ma-type)))

(defmethod trix ((series time-series-1) start-idx end-idx &key (time-period 14))
  (create-ta-series-1 %trix series start-idx end-idx :in-args `(,time-period)))

(defmethod ult-osc ((series time-series-3) start-idx end-idx
                    &key (time-period-1 7) (time-period-2 14) (time-period-3 28))
  (create-ta-series-1
   %ult-osc series start-idx end-idx
   :in-args `(,time-period-1 ,time-period-2 ,time-period-3)))

(defmethod willr ((series time-series-3) start-idx end-idx &key (time-period 14))
  (create-ta-series-1 %willr series start-idx end-idx :in-args `(,time-period)))

;; volume

(defmethod ad ((series time-series-4) start-idx end-idx)
  (create-ta-series-1 %ad series start-idx end-idx))

(defmethod ad-osc ((series time-series-4) start-idx end-idx &key (fast-period 3) (slow-period 10))
  (create-ta-series-1 %ad-osc series start-idx end-idx :in-args `(,fast-period ,slow-period)))

(defmethod obv ((series time-series-2) start-idx end-idx)
  (create-ta-series-1 %obv series start-idx end-idx))

;; volatility

(defmethod atr ((series time-series-3) start-idx end-idx &key (time-period 14))
  (create-ta-series-1 %atr series start-idx end-idx :in-args `(,time-period)))

(defmethod natr ((series time-series-3) start-idx end-idx &key (time-period 14))
  (create-ta-series-1 %natr series start-idx end-idx :in-args `(,time-period)))

(defmethod trange ((series time-series-3) start-idx end-idx)
  (create-ta-series-1 %trange series start-idx end-idx))
