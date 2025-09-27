;; Sample lisp code for format. Long-term this file should be a *test*,
;; not just *test data*


(defun square (x)
  "Return the square of a number."
                 (* x x))

(defun factorial (n) "Compute the factorial of a non-negative integer."
  (if (zerop n)
                       1
      ;; Test comment inside factorial
(* n (factorial (1- n)))))

(defun greet (name)



    "Generate a greeting message for the given name."




    (format nil "Hello, ~a!" name))

(defun sum-list (lst)
                      "Compute the sum of a list of numbers."
  (reduce #'+ lst))

(defun find-max (lst) (reduce #'max lst)
    )

;; (defmacro with-test-output ((stream) &body body)
;;   "Macro to execute body with output directed to a temporary stream for testing."
;;   `(with-output-to-string (,stream)
;;      ,@body))
