(in-package :cl-talib-api)

(define-condition talib-error (error)
  ((text :initarg :text :reader text :initform nil))
  (:report (lambda (condition stream)
             (format stream "~a" (text condition)))))

(define-condition talib-type-error (talib-error) ())
