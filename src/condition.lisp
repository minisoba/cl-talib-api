(in-package :cl-talib-api)

(define-condition talib-error (error)
  ((text :initarg :text :reader text :initform nil))
  (:report (lambda (condition stream)
             (format stream "~a" (text condition)))))

(define-condition talib-type-error (talib-error) ())

(defun ensure-double-float (value name)
  (unless (realp value)
    (error 'talib-type-error
           :text (format nil "~a must be a real number, got ~a (~a)" name value (type-of value))))
  (if (typep value 'double-float)
      value
      (coerce value 'double-float)))
