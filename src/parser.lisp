(defpackage #:scrawl.parser
  (:use :cl :smug))

(in-package :scrawl.parser)

(defconstant +sexp-open+ #\[)
(defconstant +sexp-close+ #\])

(defvar *previous-readtables* nil)

(defmethod input-empty-p ((input stream))
  (if (listen input)
      nil t))

(defmethod input-first ((input stream))
  (let ((pos (file-position input))
        (char (read-char input nil nil t)))
    (file-position input pos)
    char))

(defmethod input-rest ((input stream))
  (read-char input nil nil t)
  input)

(defun read-next ()
  (.map 'list (.is-not #'char= +sexp-close+)))

(defun read-delimiter (stream char)
  (declare (ignore stream char))
  (error "Unmatched bracket"))

(defun read-sexp (stream char)
  (declare (ignore char))
  (let ((results (parse (read-next) stream)))
    (push 'vector results)))

(defmacro enable-scrawl ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (push *readtable* *previous-readtables*)
    (setq *readtable* (copy-readtable))
    (set-macro-character +sexp-open+ 'read-sexp)
    (set-macro-character +sexp-close+ 'read-delimiter)))

(defmacro disable-scrawl ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (setq *readtable* (pop *previous-readtables*))))
