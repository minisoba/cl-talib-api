(in-package :cl-talib-api)

(defclass time-series () ())

(defmethod print-object ((obj time-series) stream)
  (let ((class (class-of obj)))
    (format stream "#<~a: ~{~a~^ ~}>"
            (class-name class)
            (loop for slot in (closer-mop:class-direct-slots class)
                  collect (format nil "~a=~a"
                                  (closer-mop:slot-definition-name slot)
                                  (slot-value obj (closer-mop:slot-definition-name slot)))))))

(defclass time-series-1 (time-series)
  ((series-1 :initarg :series-1 :reader series-1-of)))

(defun make-time-series-1 (series-1)
  (make-instance 'time-series-1 :series-1 series-1))

(defclass time-series-2 (time-series)
  ((series-1 :initarg :series-1 :reader series-1-of)
   (series-2 :initarg :series-2 :reader series-2-of)))

(defun make-time-series-2 (series-1 series-2)
  (make-instance 'time-series-2 :series-1 series-1 :series-2 series-2))

(defclass time-series-3 (time-series)
  ((series-1 :initarg :series-1 :reader series-1-of)
   (series-2 :initarg :series-2 :reader series-2-of)
   (series-3 :initarg :series-3 :reader series-3-of)))

(defun make-time-series-3 (series-1 series-2 series-3)
  (make-instance 'time-series-3 :series-1 series-1 :series-2 series-2 :series-3 series-3))

(defclass time-series-4 (time-series)
  ((series-1 :initarg :series-1 :reader series-1-of)
   (series-2 :initarg :series-2 :reader series-2-of)
   (series-3 :initarg :series-3 :reader series-3-of)
   (series-4 :initarg :series-4 :reader series-4-of)))

(defun make-time-series-4 (series-1 series-2 series-3 series-4)
  (make-instance 'time-series-4 :series-1 series-1 :series-2 series-2 :series-3 series-3 :series-4 series-4))

(defun %get-series-args (series)
  (ecase (type-of series)
    (time-series-1 (list (series-1-of series)))
    (time-series-2 (list (series-1-of series) (series-2-of series)))
    (time-series-3 (list (series-1-of series) (series-2-of series) (series-3-of series)))
    (time-series-4 (list (series-1-of series) (series-2-of series) (series-3-of series) (series-4-of series)))))

(defun %get-output-series (size count &optional (alloc-type :double))
  (mapcar (lambda (_) (cffi:foreign-alloc alloc-type :count count)) (make-list size)))

(defmacro %create-ta-series (fn series start-idx end-idx output-size &key in-args)
  (with-unique-names (args outputs ta-series start-pos nb-element count)
    `(let* ((,args (append (list ,start-idx ,end-idx)
                           (%get-series-args ,series)
                           ,in-args))
            ,start-pos ,nb-element ,outputs)
       (unwind-protect
            (progn
              (setf ,outputs (%get-output-series ,output-size (1+ ,end-idx)))
              (setf ,start-pos (cffi:foreign-alloc :int :count 1))
              (setf ,nb-element (cffi:foreign-alloc :int :count 1))

              (apply #',fn (append ,args (list ,start-pos ,nb-element) ,outputs))
              (let* ((,count (min ,end-idx
                                  (1- (cffi:mem-ref ,nb-element :int))))
                     (,ta-series (mapcar (lambda (i)
                                           (mapcar (lambda (out)
                                                     (cffi:mem-aref out :double i))
                                                   ,outputs))
                                         (iota ,count))))
                ,ta-series))
         (mapc #'cffi:foreign-free ,outputs)
         (cffi:foreign-free ,start-pos)
         (cffi:foreign-free ,nb-element)))))

(defmacro create-ta-series-1 (fn series start-idx end-idx &key in-args)
  `(%create-ta-series ,fn ,series ,start-idx ,end-idx 1 :in-args ,in-args))

(defmacro create-ta-series-2 (fn series start-idx end-idx &key in-args)
  `(%create-ta-series ,fn ,series ,start-idx ,end-idx 2 :in-args ,in-args))

(defmacro create-ta-series-3 (fn series start-idx end-idx &key in-args)
  `(%create-ta-series ,fn ,series ,start-idx ,end-idx 3 :in-args ,in-args))
