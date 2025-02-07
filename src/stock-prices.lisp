(in-package :cl-talib-api)

(defun make-foreign-object (series)
  (cffi:foreign-alloc :double :initial-contents series))

(defun free-foreign-object (series)
  (cffi:foreign-free series))

(defclass stock-prices ()
  ((open-prices  :initarg :open-prices  :reader open-prices-of  :initform nil)
   (high-prices  :initarg :high-prices  :reader high-prices-of  :initform nil)
   (low-prices   :initarg :low-prices   :reader low-prices-of   :initform nil)
   (close-prices :initarg :close-prices :reader close-prices-of :initform nil)
   (volumes      :initarg :volumes      :reader volumes-of      :initform nil)))

(defun make-stock-prices (&key open-prices high-prices low-prices close-prices volumes)
  (make-instance 'stock-prices
                 :open-prices  (make-foreign-object open-prices)
                 :high-prices  (make-foreign-object high-prices)
                 :low-prices   (make-foreign-object low-prices)
                 :close-prices (make-foreign-object close-prices)
                 :volumes      (make-foreign-object volumes)))

(defmethod destroy ((obj stock-prices))
  (free-foreign-object (open-prices-of  obj))
  (free-foreign-object (high-prices-of  obj))
  (free-foreign-object (low-prices-of   obj))
  (free-foreign-object (close-prices-of obj))
  (free-foreign-object (volumes-of      obj)))
