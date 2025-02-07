(asdf:defsystem :cl-talib-api-test
    :author "Y. IGUCHI"
    :license "MIT"
    :description "Unit tests for cl-talib-api"
    :serial t
    :depends-on (:alexandria
                 :cl-ppcre
                 :fiveam
                 :local-time
                 :parse-float
                 :read-csv
                 :yason)
    :components ((:file "t/package")
                 (:file "t/api"))
    :perform (asdf:test-op
              (o s)
              (uiop:symbol-call
               :cl-talib-api.test '#:run-test)))
