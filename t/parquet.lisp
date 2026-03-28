(in-package :cl-talib-api.test)

;;; ----- test helper classes -----

(defclass test-record ()
  ((id    :initarg :id    :reader test-record-id-of    :initform 0)
   (value :initarg :value :reader test-record-value-of :initform 0.0)
   (label :initarg :label :reader test-record-label-of :initform ""))
  (:documentation "Simple CLOS class with BIGINT, DOUBLE and VARCHAR slots."))

(defclass test-record-ts ()
  ((id      :initarg :id      :reader test-record-ts-id-of      :initform 0)
   (created :initarg :created :reader test-record-ts-created-of
            :initform (local-time:universal-to-timestamp 0)))
  (:documentation "CLOS class with a TIMESTAMP slot for type-mapping tests."))

(defclass other-record ()
  ((name :initarg :name :reader other-record-name-of :initform ""))
  (:documentation "A different class used to verify heterogeneous-object rejection."))

;;; ----- suite -----

(def-suite* parquet-test :in cl-talib-api-test)

;;; ----- %column-name -----

(test column-name-converts-kebab-to-snake
  (is (string= "close_prices" (cl-talib-api::%column-name 'close-prices)))
  (is (string= "id"           (cl-talib-api::%column-name 'id)))
  (is (string= "my_long_name" (cl-talib-api::%column-name 'my-long-name))))

;;; ----- %quote-identifier -----

(test quote-identifier-wraps-in-double-quotes
  (is (string= "\"foo\"" (cl-talib-api::%quote-identifier "foo"))))

(test quote-identifier-doubles-embedded-quotes
  (is (string= "\"foo\"\"bar\"" (cl-talib-api::%quote-identifier "foo\"bar"))))

;;; ----- %slot-definitions -----

(test slot-definitions-maps-initform-to-sql-type
  (let* ((obj  (make-instance 'test-record))
         (defs (cl-talib-api::%slot-definitions obj)))
    (is (= 3 (length defs)))
    (is (string= "BIGINT"  (cdr (assoc 'id    defs))))
    (is (string= "DOUBLE"  (cdr (assoc 'value defs))))
    (is (string= "VARCHAR" (cdr (assoc 'label defs))))))

(test slot-definitions-recognises-timestamp
  (let* ((obj  (make-instance 'test-record-ts))
         (defs (cl-talib-api::%slot-definitions obj)))
    (is (string= "BIGINT"    (cdr (assoc 'id      defs))))
    (is (string= "TIMESTAMP" (cdr (assoc 'created defs))))))

;;; ----- %slot-values -----

(test slot-values-returns-alist-of-values
  (let* ((obj  (make-instance 'test-record :id 42 :value 3.14d0 :label "hello"))
         (vals (cl-talib-api::%slot-values obj)))
    (is (= 42     (cdr (assoc 'id    vals))))
    (is (= 3.14d0 (cdr (assoc 'value vals))))
    (is (string= "hello" (cdr (assoc 'label vals))))))

;;; ----- %sql-escape -----

(test sql-escape-integer
  (is (string= "42" (cl-talib-api::%sql-escape 42))))

(test sql-escape-float
  (let ((result (cl-talib-api::%sql-escape 3.14d0)))
    (is (stringp result))
    (is (search "3.14" result))))

(test sql-escape-null
  (is (string= "NULL" (cl-talib-api::%sql-escape nil))))

(test sql-escape-string
  (is (string= "'hello'" (cl-talib-api::%sql-escape "hello"))))

(test sql-escape-string-with-single-quote
  (is (string= "'it''s'" (cl-talib-api::%sql-escape "it's"))))

(test sql-escape-timestamp
  (let* ((ts     (local-time:universal-to-timestamp 0))
         (result (cl-talib-api::%sql-escape ts)))
    (is (char= #\' (char result 0)))
    (is (char= #\' (char result (1- (length result)))))))

;;; ----- %make-create-table-sql -----

(test make-create-table-sql-contains-expected-fragments
  (let* ((obj (make-instance 'test-record))
         (sql (cl-talib-api::%make-create-table-sql "my_table" obj)))
    (is (search "CREATE TABLE" sql))
    (is (search "\"my_table\"" sql))
    (is (search "BIGINT"  sql))
    (is (search "DOUBLE"  sql))
    (is (search "VARCHAR" sql))))

;;; ----- %make-insert-sql -----

(test make-insert-sql-contains-expected-fragments
  (let* ((obj (make-instance 'test-record :id 1 :value 2.5d0 :label "test"))
         (sql (cl-talib-api::%make-insert-sql "my_table" obj)))
    (is (search "INSERT INTO" sql))
    (is (search "\"my_table\"" sql))
    (is (search "VALUES" sql))
    (is (search "'test'" sql))))

;;; ----- save-to-parquet -----

(defun %temp-parquet-path (tag)
  "Return a unique temporary file path for parquet tests."
  (namestring
   (merge-pathnames
    (format nil "cl-talib-test-~a-~a-~a.parquet" tag (get-universal-time) (random 1000000))
    #P"/tmp/")))

(test save-to-parquet-nil-returns-nil
  (is (null (save-to-parquet nil "/tmp/cl-talib-test-nil.parquet"))))

(test save-to-parquet-writes-file
  (let ((path (%temp-parquet-path "write")))
    (when (probe-file path) (delete-file path))
    (unwind-protect
         (progn
           (save-to-parquet
            (list (make-instance 'test-record :id 1 :value 1.0d0 :label "a")
                  (make-instance 'test-record :id 2 :value 2.0d0 :label "b"))
            path)
           (is-true (probe-file path)))
      (when (probe-file path) (delete-file path)))))

(test save-to-parquet-deduplicates-on-repeated-write
  (let ((path (%temp-parquet-path "dedup")))
    (when (probe-file path) (delete-file path))
    (unwind-protect
         (let ((objs (list (make-instance 'test-record :id 1 :value 1.0d0 :label "a"))))
           (save-to-parquet objs path)
           (save-to-parquet objs path)
           (duckdb:with-open-database (db)
             (duckdb:with-open-connection (conn db)
               (let ((result (duckdb:query
                              (format nil "SELECT count(*) FROM read_parquet('~a')" path)
                              nil :connection conn)))
                 (is (= 1 (caar result)))))))
      (when (probe-file path) (delete-file path)))))

(test save-to-parquet-rejects-heterogeneous-objects
  (let ((path (%temp-parquet-path "hetero")))
    (when (probe-file path) (delete-file path))
    (unwind-protect
         (signals error
           (save-to-parquet
            (list (make-instance 'test-record :id 1 :value 1.0d0 :label "a")
                  (make-instance 'other-record :name "b"))
            path))
      (when (probe-file path) (delete-file path)))))
