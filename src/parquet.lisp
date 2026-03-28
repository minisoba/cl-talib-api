(in-package :cl-talib-api)

(defun %slot-definitions (obj)
  "Return an alist of (SLOT-NAME . SQL-TYPE) for the direct slots of OBJ's class."
  (loop for slot in (closer-mop:class-direct-slots (class-of obj))
        for name = (closer-mop:slot-definition-name slot)
        for initform = (closer-mop:slot-definition-initform slot)
        collect (cons name
                      (cond ((eql initform 0)   "BIGINT")
                            ((eql initform 0.0) "DOUBLE")
                            ((and (listp initform)
                                  (eq (first initform) 'local-time:universal-to-timestamp))
                             "TIMESTAMP")
                            (t                  "VARCHAR")))))

(defun %slot-values (obj)
  "Return an alist of (SLOT-NAME . VALUE) for the direct slots of OBJ."
  (loop for slot in (closer-mop:class-direct-slots (class-of obj))
        for name = (closer-mop:slot-definition-name slot)
        collect (cons name (slot-value obj name))))

(defun %make-create-table-sql (table-name obj)
  "Generate a CREATE TABLE SQL statement from OBJ's slot definitions."
  (format nil "CREATE TABLE ~a (~{~a~^, ~})"
          table-name
          (loop for (name . sql-type) in (%slot-definitions obj)
                collect (format nil "~a ~a"
                                (substitute #\_ #\- (string-downcase (symbol-name name)))
                                sql-type))))

(defun %sql-escape (value)
  "Escape VALUE for safe inclusion in a SQL statement."
  (cond ((floatp value)  (format nil "~f" value))
        ((numberp value) (format nil "~a" value))
        ((null value)    "NULL")
        ((typep value 'local-time:timestamp)
         (format nil "'~a'" (local-time:format-timestring nil value)))
        (t (format nil "'~a'" (cl-ppcre:regex-replace-all "'" (princ-to-string value) "''")))))

(defun %make-insert-sql (table-name obj)
  "Generate an INSERT INTO SQL statement from OBJ's slot values."
  (let ((cols (%slot-definitions obj))
        (vals (%slot-values obj)))
    (format nil "INSERT INTO ~a VALUES (~{~a~^, ~})"
            table-name
            (loop for (name . _) in cols
                  for val = (cdr (assoc name vals))
                  collect (%sql-escape val)))))

(defun save-to-parquet (objects parquet-path &key (table-name "talib_data"))
  "Write a list of CLOS objects to a Parquet file via DuckDB.
Each object's direct slots are mapped to columns. All objects must share the
same type and slot layout.  If PARQUET-PATH already exists, existing rows are
loaded first and the result is deduplicated so repeated writes do not create
duplicate records."
  (when (null objects) (return-from save-to-parquet nil))
  (let* ((first-obj (first objects))
         (expected-slot-defs (%slot-definitions first-obj))
         (expected-type (type-of first-obj))
         (existing-p (probe-file parquet-path)))
    (duckdb:with-open-database (db)
      (duckdb:with-open-connection (conn db)
        (duckdb:query (%make-create-table-sql table-name first-obj) nil :connection conn)
        (when existing-p
          (let* ((column-names (mapcar (lambda (slot-def)
                                          (substitute #\_ #\- (string-downcase (symbol-name (car slot-def)))))
                                        expected-slot-defs))
                 (column-list (format nil "~{~a~^, ~}" column-names)))
            (duckdb:query
             (format nil "INSERT INTO ~a (~a) SELECT ~a FROM read_parquet(~a)"
                     table-name
                     column-list
                     column-list
                     (%sql-escape parquet-path))
             nil :connection conn)))
        (let ((committed-p nil))
          (unwind-protect
               (progn
                 (duckdb:query "BEGIN" nil :connection conn)
                 (dolist (obj objects)
                   (unless (and (eq (type-of obj) expected-type)
                                (equal expected-slot-defs (%slot-definitions obj)))
                     (error "SAVE-TO-PARQUET expects all objects to have the same type and slot layout. Expected type ~S with slots ~S, but got type ~S with slots ~S."
                            expected-type
                            (mapcar #'car expected-slot-defs)
                            (type-of obj)
                            (mapcar #'car (%slot-definitions obj))))
                   (duckdb:query (%make-insert-sql table-name obj) nil :connection conn))
                 (duckdb:query "COMMIT" nil :connection conn)
                 (setf committed-p t))
            (unless committed-p
              (ignore-errors
                (duckdb:query "ROLLBACK" nil :connection conn)))))
        (duckdb:query
         (format nil "COPY (SELECT DISTINCT * FROM ~a) TO ~a (FORMAT PARQUET)"
                 table-name
                 (%sql-escape parquet-path))
         nil :connection conn)))))
