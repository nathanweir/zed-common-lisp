(ql:quickload "alive-lsp")

(defpackage #:cl-formatter
    (:use #:cl)
    (:export #:main))
(in-package #:cl-formatter)

;; (in-package cl-formatter)

(defparameter *out-path* "out.lisp")

(defun main (file-path)
    (format t "Formatting ~A to ~A ~%" file-path *out-path*)
    (write-formatted-file file-path *out-path*))

(defun write-formatted-file (file-path out-path)
    "Formats the file at file-path and then writes it to out-path"
    (with-open-file (in file-path)
      (let ((input-string (with-output-to-string (out)
                            (loop for line = (read-line in nil)
                                  while line
                                  do (progn
                                       (princ line out)
                                       (terpri out))))))
      (format t "Input string read from ~A:~%~A~%" file-path input-string)
      (let ((result (format-lisp-code input-string)))
          ;; Write the result
            (with-open-file (out out-path :direction :output :if-does-not-exist :create :if-exists :supersede)
                (princ result out))))))

;; could use alive/postion
(defun create-pos (line col)
    (list (cons :line line)
          (cons :character col)))

;; could use alive/range
(defun create-range (start end)
    (list (cons :start start)
          (cons :end end)))


(defun edit-input (edits input-string)
  "Apply EDITS to INPUT-STRING and return the modified string."
  (let* ((lines (split-sequence:split-sequence #\Newline input-string))
         (result lines))
    (dolist (edit edits)
      ;; Print all key-value pairs in the edit hash table
      (maphash (lambda (key value)
                 (format t "~A => ~A~%" key value))
               edit)
      (let ((range (gethash "range" edit))
          (text (gethash "text" edit)))
        (format t "Applying edit: range=~A, text=~A~%" range text)
        (let ((start (cdr (assoc :start range)))
            (end (cdr (assoc :end range))))
        (let* ((start-line (cdr (assoc :line start)))
            (start-char (cdr (assoc :character start)))
            (end-line (cdr (assoc :line end)))
            (end-char (cdr (assoc :character end))))
        ;; Get the text before, replace, and after
        (let ((before (subseq (nth start-line result) 0 start-char))
                (after (subseq (nth end-line result) end-char)))
            ;; Replace lines in result
            (setf (nth start-line result)
                (concatenate 'string before text after))
            ;; Remove lines between start and end if needed
            (loop for i from (1+ start-line) to end-line
                do (setf (nth i result) "")))))))
    (format nil "~{~A~^~%~}" result)))



(defun format-lisp-code (input-string)
  "Auto-format Common Lisp code from INPUT-STRING."
  (with-input-from-string (stream input-string)
  ;; These "edits" are text changes to be made at specific
  ;; start and end positions (line & char). For example:
  ;;   range => ((START (LINE . 9) (CHARACTER . 15))
  ;;             (END (LINE . 10) (CHARACTER . 23)))
  ;; text =>
  ;; (text is empty to signify deleting the chars in that range)
  (let ((edits
    (alive/format:range stream
        (create-range (create-pos 0 0)
            (create-pos 26 10)))))
        (edit-input edits input-string))))
