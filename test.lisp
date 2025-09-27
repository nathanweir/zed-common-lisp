;;;; Complete Common Lisp Syntax Example
;;;; Multi-line comment block
;;;; Testing all major language constructs

;;; Package definition and exports
(defpackage :syntax-test
  (:use :common-lisp)
  (:export #:main-function #:*global-var* #:test-class))

(in-package :syntax-test)

(+ 1 2)

(defun do-thing ()
	(* 2 3))

(do-thing)

;;; Global variables and constants
(defvar *global-counter* 0 "A global counter variable")
(defparameter *debug-mode* t "Debug mode flag")
(defconstant +pi+ 3.14159265359 "Mathematical constant pi")

;;; Macros
(defmacro when-debug (&body body)
  "Execute body only when debug mode is enabled"
  `(when *debug-mode*
     ,@body))

(defmacro with-timing ((var) &body body)
  "Time the execution of body"
  `(let ((start (get-internal-real-time)))
     (prog1 (progn ,@body)
       (setf ,var (- (get-internal-real-time) start)))))

;;; Function definitions
(defun factorial (n)
  "Calculate factorial recursively"
  (declare (type (integer 0) n)
           (optimize (speed 3) (safety 1)))
  (if (<= n 1)
      1
      (* n (factorial (1- n)))))

(defun fibonacci (n &optional (a 0) (b 1))
  "Calculate nth Fibonacci number with optional tail recursion"
  (cond ((zerop n) a)
        ((= n 1) b)
        (t (fibonacci (1- n) b (+ a b)))))

;;; Generic functions and methods
(defgeneric area (shape)
  (:documentation "Calculate area of a geometric shape"))

(defmethod area ((shape number))
  "Area of a square with given side length"
  (* shape shape))

(defmethod area ((shape cons))
  "Area of rectangle given as (width . height)"
  (* (car shape) (cdr shape)))

;;; Classes and structures
(defclass geometric-shape ()
  ((name :initarg :name
         :accessor shape-name
         :type string
         :documentation "Name of the shape")
   (color :initarg :color
          :accessor shape-color
          :initform "white"))
  (:documentation "Base class for geometric shapes"))

(defclass circle (geometric-shape)
  ((radius :initarg :radius
           :accessor circle-radius
           :type number))
  (:default-initargs :name "Circle"))

(defstruct point
  "A 2D point structure"
  (x 0.0 :type single-float)
  (y 0.0 :type single-float))

;;; Method specialization
(defmethod area ((circle circle))
  "Calculate area of a circle"
  (* +pi+ (expt (circle-radius circle) 2)))

(defmethod print-object ((circle circle) stream)
  (format stream "#<CIRCLE :name ~A :radius ~A :color ~A>"
          (shape-name circle)
          (circle-radius circle)
          (shape-color circle)))

;;; Conditions and error handling
(define-condition invalid-input-error (error)
  ((input :initarg :input :reader invalid-input))
  (:report (lambda (condition stream)
             (format stream "Invalid input: ~A"
                     (invalid-input condition)))))

(defun safe-divide (a b)
  "Safely divide two numbers with error handling"
  (handler-case
      (progn
        (when (zerop b)
          (error 'invalid-input-error :input b))
        (/ a b))
    (division-by-zero ()
      (format t "Division by zero detected~%")
      nil)
    (invalid-input-error (e)
      (format t "Error: ~A~%" e)
      nil)))

;;; Advanced control structures
(defun demonstrate-loops ()
  "Show various loop constructs"
  (let ((results '()))
    ;; LOOP macro with various clauses
    (loop for i from 1 to 10
          for j = (* i i)
          when (evenp i)
            collect j into evens
          when (oddp i)
            collect j into odds
          finally (return (list :evens evens :odds odds)))

    ;; DO loop
    (do ((i 0 (1+ i))
         (sum 0 (+ sum i)))
        ((> i 5) sum)
      (when-debug
        (format t "i=~A, sum=~A~%" i sum)))

    ;; DOLIST and DOTIMES
    (dolist (item '(a b c d))
      (push item results))

    (dotimes (i 3)
      (push i results))

    results))

;;; Lambda expressions and closures
(defun make-counter (&optional (start 0))
  "Create a closure-based counter"
  (let ((count start))
    (lambda (&optional (increment 1))
      (incf count increment))))

(defun higher-order-example ()
  "Demonstrate higher-order functions"
  (let ((numbers '(1 2 3 4 5 6 7 8 9 10)))
    (values
     ;; MAP functions
     (mapcar #'(lambda (x) (* x x)) numbers)
     (mapcan #'(lambda (x) (when (evenp x) (list x))) numbers)

     ;; REDUCE
     (reduce #'+ numbers)
     (reduce #'max numbers)

     ;; REMOVE-IF and FIND-IF
     (remove-if #'oddp numbers)
     (find-if #'(lambda (x) (> x 5)) numbers))))

;;; Multiple values and destructuring
(defun multiple-values-demo ()
  "Demonstrate multiple values and destructuring"
  (multiple-value-bind (quotient remainder)
      (floor 23 7)
    (destructuring-bind (a b &optional c &rest rest)
        '(1 2 3 4 5 6)
      (list :quotient quotient
            :remainder remainder
            :a a :b b :c c :rest rest))))

;;; Symbols, keywords, and packages
(defun symbol-examples ()
  "Various symbol and keyword examples"
  (let ((uninterned-symbol (make-symbol "TEMP"))
        (gensym-symbol (gensym "G")))
    (list :keyword :another-keyword
          :uninterned uninterned-symbol
          :gensym gensym-symbol
          :qualified-symbol 'common-lisp:car
          :symbol-properties (symbol-plist 'test-symbol))))

;;; Arrays and sequences
(defun array-examples ()
  "Demonstrate array operations"
  (let ((vector #(1 2 3 4 5))
        (2d-array (make-array '(3 3) :initial-element 0))
        (string-array #("hello" "world" "lisp")))

    ;; Array access and modification
    (setf (aref 2d-array 1 1) 42)
    (setf (aref vector 0) 99)

    (format t "string-array is: ~A~%" string-array)

    ;; Sequence operations
    (list :vector vector
          :2d-array 2d-array
          :length (length vector)
          :subseq (subseq vector 1 4)
          :concatenate (concatenate 'vector vector #(6 7 8)))))

;;; Hash tables
(defun hash-table-demo ()
  "Hash table operations"
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash "key1" ht) "value1"
          (gethash "key2" ht) "value2"
          (gethash 42 ht) "numeric-key")

    (maphash #'(lambda (k v)
                 (format t "~A -> ~A~%" k v))
             ht)
    ht))

(getha)

;;; Input/Output and format strings
(defun io-examples ()
  "Demonstrate I/O operations"
  (with-output-to-string (stream)
    (format stream "~&Numbers: ~{~A~^, ~}~%" '(1 2 3 4 5))
    (format stream "~&Formatted: ~10A ~5D ~$~%" "test" 42 3.14159)
    (format stream "~&Conditional: ~:[false~;true~]~%" t)
    (format stream "~&Iteration: ~{Item ~A~%~}" '(1 2 3))))

;;; Reader macros and special syntax
(defun reader-macro-examples ()
  "Examples of reader macros and special syntax"
  (list :quote 'symbol
        :function #'car
        :backquote `(list ,@'(1 2 3) ,(+ 2 3))
        :character #\A
        :string "Hello, World!"
        :pathname #P"/tmp/test.txt"
        :complex #C(3 4)
        :bit-vector #*10110
        :radix-numbers (list #b1010 #o777 #xFF #36rZZ)))

;;; CLOS advanced features
(defclass test-class ()
  ((slot1 :allocation :class
          :initform "class-allocated")
   (slot2 :allocation :instance
          :accessor test-slot2)))

(defmethod initialize-instance :after ((obj test-class) &key)
  (format t "Created instance of test-class~%"))

(define-hash-table-test)

;;; Main demonstration function
(defun main-function ()
  "Main function demonstrating all features"
  (let ((timing 0))
    (with-timing (timing)
      (when-debug
        (format t "Starting comprehensive syntax test~%"))

      ;; Test all functions
      (factorial 5)
      (fibonacci 10)
      (safe-divide 10 2)
      (demonstrate-loops)
      (higher-order-example)
      (multiple-values-demo)
      (symbol-examples)
      (array-examples)
      (hash-table-demo)
      (io-examples)
      (reader-macro-examples)

      ;; Create objects
      (let ((circle (make-instance 'circle :radius 5.0 :color "red"))
            (point (make-point :x 1.0 :y 2.0))
            (counter (make-counter 10)))

        (list :circle-area (area circle)
              :point point
              :counter-value (funcall counter 5)
              :timing timing)))))



;;; Compile-time computations
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +compile-time-constant+
    (factorial 5)
    "Computed at compile time"))

#|
Block comment demonstrating
multiple lines and nested
#| comments |#
for comprehensive testing
|#

;; Final form - package cleanup would go here in real code
(provide :syntax-test)
