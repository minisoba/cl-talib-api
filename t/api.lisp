(in-package :cl-talib-api.test)

(defclass mock-stock-prices (stock-prices)
  ((mavp-periods :initarg :mavp-periods :reader mavp-periods-of :initform nil)))

(defun make-mock-stock-prices (&key open-prices high-prices low-prices close-prices volumes mavp-periods)
  (make-instance 'mock-stock-prices
                 :open-prices  (make-foreign-object open-prices)
                 :high-prices  (make-foreign-object high-prices)
                 :low-prices   (make-foreign-object low-prices)
                 :close-prices (make-foreign-object close-prices)
                 :volumes      (make-foreign-object volumes)
                 :mavp-periods (make-foreign-object mavp-periods)))

(defvar *end-idx* 0)
(defvar *stock-prices* nil)

(defun %get-csv-file-path (filename)
  (merge-pathnames filename (asdf:system-relative-pathname "cl-talib-api" "t/data/")))

(defun %read-expected-from-csv (csv)
  (mapcar #'(lambda (line)
              (loop for x in (cl-ppcre:split "," line)
                    collect (parse-float:parse-float x :type 'double-float)))
          (uiop:read-file-lines (cl-talib-api.test::%get-csv-file-path csv))))

(defun %nearest-equal-or-less (series expected pos &optional (precision 5))
  (let ((our (nth pos series))
        (expect (nth pos expected)))
    (< (- our expect) (expt 10 (* -1 precision)))))

(defmacro %compare-with-expected-from-csv (fn csv ts &optional additional-params)
  (with-unique-names (params series expected)
    `(let* ((,params   (append (list ,ts 0 *end-idx*) ,additional-params))
            (,series   (apply ,fn ,params))
            (,expected (%read-expected-from-csv ,csv)))
       (dotimes (i (length ,expected))
         (let ((row1 (nth i ,series))
               (row2 (nth i ,expected)))
           (dotimes (j (length row2))
             (is (%nearest-equal-or-less row1 row2 j))))))))

(def-suite cl-talib-api-test)

(def-suite* api-test :in cl-talib-api-test)

(test accbands
  (let ((ts (make-time-series-3
             (high-prices-of  *stock-prices*)
             (low-prices-of   *stock-prices*)
             (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 'accbands "accbands.csv" ts)))

(test bbands
  (let ((ts (make-time-series-1 (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 'bbands "bbands.csv" ts)))

(test dema
  (let ((ts (make-time-series-1 (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 'dema "dema.csv" ts)))

(test ema
  (let ((ts (make-time-series-1 (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 'ema "ema.csv" ts)))

(test kama
  (let ((ts (make-time-series-1 (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 'kama "kama.csv" ts)))

(test ma
  (let ((ts (make-time-series-1 (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 'ma "ma.csv" ts)))

#-unix
(test mama
  (let ((ts (make-time-series-1 (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv
     'mama "mama.csv" ts
     '(:fast-limit 0.05 :slow-limit 0.2))))

(test mavp
  (let ((ts (make-time-series-2
             (close-prices-of *stock-prices*)
             (mavp-periods-of *stock-prices*))))
    (%compare-with-expected-from-csv
     'mavp "mavp.csv" ts
     '(:min-period 2 :max-period 10))))

(test midpoint
  (let ((ts (make-time-series-1 (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 'midpoint "midpoint.csv" ts)))

(test midprice
  (let ((ts (make-time-series-2
             (high-prices-of *stock-prices*)
             (low-prices-of  *stock-prices*))))
    (%compare-with-expected-from-csv 'midprice "midprice.csv" ts)))

(test sar
  (let ((ts (make-time-series-2
             (high-prices-of *stock-prices*)
             (low-prices-of  *stock-prices*))))
    (%compare-with-expected-from-csv 'sar "sar.csv" ts)))

(test sar-ext
  (let ((ts (make-time-series-2
             (high-prices-of *stock-prices*)
             (low-prices-of  *stock-prices*))))
    (%compare-with-expected-from-csv 'sar-ext "sar-ext.csv" ts)))

(test sma
  (let ((ts (make-time-series-1 (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 'sma "sma.csv" ts)))

(test t3
  (let ((ts (make-time-series-1 (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 't3 "t3.csv" ts)))

(test tema
  (let ((ts (make-time-series-1 (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 'tema "tema.csv" ts)))

(test trima
  (let ((ts (make-time-series-1 (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 'trima "trima.csv" ts)))

(test wma
  (let ((ts (make-time-series-1 (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 'wma "wma.csv" ts)))

(test adx
  (let ((ts (make-time-series-3
             (high-prices-of  *stock-prices*)
             (low-prices-of   *stock-prices*)
             (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 'adx "adx.csv" ts)))

(test adxr
  (let ((ts (make-time-series-3
             (high-prices-of  *stock-prices*)
             (low-prices-of   *stock-prices*)
             (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 'adxr "adxr.csv" ts)))

(test apo
  (let ((ts (make-time-series-1 (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 'apo "apo.csv" ts)))

(test aroon
  (let ((ts (make-time-series-2
             (high-prices-of *stock-prices*)
             (low-prices-of  *stock-prices*))))
    (%compare-with-expected-from-csv 'aroon "aroon.csv" ts)))

(test aroon-osc
  (let ((ts (make-time-series-2
             (high-prices-of *stock-prices*)
             (low-prices-of  *stock-prices*))))
    (%compare-with-expected-from-csv 'aroon-osc "aroon-osc.csv" ts)))

(test bop
  (let ((ts (make-time-series-4
             (open-prices-of  *stock-prices*)
             (high-prices-of  *stock-prices*)
             (low-prices-of   *stock-prices*)
             (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 'bop "bop.csv" ts)))

(test cci
  (let ((ts (make-time-series-3
             (high-prices-of  *stock-prices*)
             (low-prices-of   *stock-prices*)
             (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 'cci "cci.csv" ts)))

(test cmo
  (let ((ts (make-time-series-1 (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 'cmo "cmo.csv" ts)))

(test dx
  (let ((ts (make-time-series-3
             (high-prices-of  *stock-prices*)
             (low-prices-of   *stock-prices*)
             (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 'dx "dx.csv" ts)))

(test macd
  (let ((ts (make-time-series-1 (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 'macd "macd.csv" ts)))

(test macd-ext
  (let ((ts (make-time-series-1 (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 'macd-ext "macd-ext.csv" ts)))

(test macd-fix
  (let ((ts (make-time-series-1 (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 'macd-fix "macd-fix.csv" ts)))

(test mfi
  (let ((ts (make-time-series-4
             (high-prices-of  *stock-prices*)
             (low-prices-of   *stock-prices*)
             (close-prices-of *stock-prices*)
             (volumes-of      *stock-prices*))))
    (%compare-with-expected-from-csv 'mfi "mfi.csv" ts)))

(test minus-di
  (let ((ts (make-time-series-3
             (high-prices-of  *stock-prices*)
             (low-prices-of   *stock-prices*)
             (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 'minus-di "minus-di.csv" ts)))

(test minus-dm
  (let ((ts (make-time-series-2
             (high-prices-of *stock-prices*)
             (low-prices-of  *stock-prices*))))
    (%compare-with-expected-from-csv 'minus-dm "minus-dm.csv" ts)))

(test mom
  (let ((ts (make-time-series-1 (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 'mom "mom.csv" ts)))

(test plus-di
  (let ((ts (make-time-series-3
             (high-prices-of  *stock-prices*)
             (low-prices-of   *stock-prices*)
             (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 'plus-di "plus-di.csv" ts)))

(test plus-dm
  (let ((ts (make-time-series-2
             (high-prices-of *stock-prices*)
             (low-prices-of  *stock-prices*))))
    (%compare-with-expected-from-csv 'plus-dm "plus-dm.csv" ts)))

(test ppo
  (let ((ts (make-time-series-1 (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 'ppo "ppo.csv" ts)))

(test roc
  (let ((ts (make-time-series-1 (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 'roc "roc.csv" ts)))

(test rocp
  (let ((ts (make-time-series-1 (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 'rocp "rocp.csv" ts)))

(test rocr
  (let ((ts (make-time-series-1 (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 'rocr "rocr.csv" ts)))

(test rocr100
  (let ((ts (make-time-series-1 (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 'rocr100 "rocr100.csv" ts)))

(test rsi
  (let ((ts (make-time-series-1 (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 'rsi "rsi.csv" ts)))

(test stoch
  (let ((ts (make-time-series-3
             (high-prices-of  *stock-prices*)
             (low-prices-of   *stock-prices*)
             (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 'stoch "stoch.csv" ts)))

(test stochf
  (let ((ts (make-time-series-3
             (high-prices-of  *stock-prices*)
             (low-prices-of   *stock-prices*)
             (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 'stochf "stochf.csv" ts)))

(test stoch-rsi
  (let ((ts (make-time-series-1
             (close-prices-of  *stock-prices*))))
    (%compare-with-expected-from-csv 'stoch-rsi "stoch-rsi.csv" ts)))

(test trix
  (let ((ts (make-time-series-1 (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 'trix "trix.csv" ts)))

(test ult-osc
  (let ((ts (make-time-series-3
             (high-prices-of  *stock-prices*)
             (low-prices-of   *stock-prices*)
             (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 'ult-osc "ult-osc.csv" ts)))

(test willr
  (let ((ts (make-time-series-3
             (high-prices-of  *stock-prices*)
             (low-prices-of   *stock-prices*)
             (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 'willr "willr.csv" ts)))

(test ad
  (let ((ts (make-time-series-4
             (high-prices-of  *stock-prices*)
             (low-prices-of   *stock-prices*)
             (close-prices-of *stock-prices*)
             (volumes-of      *stock-prices*))))
    (%compare-with-expected-from-csv 'ad "ad.csv" ts)))

(test ad-osc
  (let ((ts (make-time-series-4
             (high-prices-of  *stock-prices*)
             (low-prices-of   *stock-prices*)
             (close-prices-of *stock-prices*)
             (volumes-of      *stock-prices*))))
    (%compare-with-expected-from-csv 'ad-osc "ad-osc.csv" ts)))

(test obv
  (let ((ts (make-time-series-2
             (close-prices-of *stock-prices*)
             (volumes-of      *stock-prices*))))
    (%compare-with-expected-from-csv 'obv "obv.csv" ts)))

(test atr
  (let ((ts (make-time-series-3
             (high-prices-of  *stock-prices*)
             (low-prices-of   *stock-prices*)
             (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 'atr "atr.csv" ts)))

(test natr
  (let ((ts (make-time-series-3
             (high-prices-of  *stock-prices*)
             (low-prices-of   *stock-prices*)
             (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 'natr "natr.csv" ts)))

(test trange
  (let ((ts (make-time-series-3
             (high-prices-of  *stock-prices*)
             (low-prices-of   *stock-prices*)
             (close-prices-of *stock-prices*))))
    (%compare-with-expected-from-csv 'trange "trange.csv" ts)))

(test correl
  )

(defun %make-stock-prices-from-csv (csv)
  (flet ((%stof (x)
           (parse-float:parse-float x :type 'double-float)))
    (let ((csv-data (with-open-file (stream (%get-csv-file-path csv))
                      (cdr (read-csv:parse-csv stream)))))
      (let (open-prices high-prices low-prices close-prices volumes mavp-periods)
        (dolist (row (reverse csv-data))
          (let ((open%   (%stof (nth 0 row)))
                (high%   (%stof (nth 1 row)))
                (low%    (%stof (nth 2 row)))
                (close%  (%stof (nth 3 row)))
                (volume% (%stof (nth 4 row))))
          (push open%   open-prices)
          (push high%   high-prices)
          (push low%    low-prices)
          (push close%  close-prices)
          (push volume% volumes)
          (push (/ (- high% low%) close%) mavp-periods)))
        (setf *end-idx* (length close-prices))
        (make-mock-stock-prices :open-prices  open-prices
                                :high-prices  high-prices
                                :low-prices   low-prices
                                :close-prices close-prices
                                :volumes      volumes
                                :mavp-periods mavp-periods)))))

(defun load-test-data ()
  (%make-stock-prices-from-csv "stock-prices.csv"))

(defun run-test ()
  (setf *read-default-float-format* 'double-float)
  (setf *stock-prices* (load-test-data))
  (run! 'cl-talib-api-test))
