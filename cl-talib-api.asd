(asdf:defsystem :cl-talib-api
    :author "Y. IGUCHI"
    :license "MIT"
    :description "TA-LIB API for Common Lisp"
    :serial t
    :depends-on (:alexandria
                 :cffi
                 :cl-ppcre
                 :closer-mop
                 :fiveam
                 :local-time
                 :parse-float
                 :read-csv
                 :yason)
    :components ((:file "src/package")
                 (:file "src/condition")
                 (:file "src/cffi")
                 (:file "src/series")
                 (:file "src/stock-prices")
                 (:file "src/api"))
    :in-order-to ((asdf:test-op
                   (asdf:test-op :cl-talib-api-test))))
