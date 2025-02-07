(in-package :cl-user)

(defpackage :cl-talib-api.test
  (:use :cl :alexandria :fiveam :cl-talib-api)
  (:export
   #:*end-idx*
   #:load-test-data
   #:run-test))
