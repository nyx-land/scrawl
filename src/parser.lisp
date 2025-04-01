(defpackage #:scrawl.parser
  (:use :cl :smug)
  (:import-from
   :common-doc
   :reference
   :content-node
   :text-node
   :document-link
   :web-link
   :definition
   :unordered-list
   :ordered-list
   :list-item
   :definition-list
   :image
   :figure
   :row
   :table
   :section
   :document
   :children))

(in-package :scrawl.parser)

(defconstant +sexp-open+ #\[)
(defconstant +sexp-close+ #\])

(defparameter *whitespace*
  '(#\Space #\Tab #\Newline #\Return))

(defvar *previous-readtables* nil)

(defparameter *ipsum* (lorem-ipsum:paragraphs 5))

;;;; smug methods -----------------------------------------------------

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

;;;; parsers ----------------------------------------------------------

(defun tag-p (c)
  (case c
    (#\# 'section)
    (#\- 'unordered-list)
    (#\+ 'ordered-list)
    (t `(text-node ,c))))

(defun inline-p (c)
  (case c
    (#\* 'bold)
    (#\/ 'italic)
    (#\_ 'underline)
    (#\~ 'strikethrough)
    (#\$ 'code)
    (:sup 'superscript)
    (:sub 'subscript)))

(defun key-p (input)
  (if (eq #\: (car input))
      (read-from-string "~{~a~}" input)
      (format nil "~{~a~}" input)))

(defun whitespace-p ()
  (.is 'member *whitespace*))

(defun read-inline ()
  ())

(defun sexp-atom (input)
  (if input
      (if (= 1 (length input))
          (tag-p (car input))
          (tag-p (key-p input)))
      '(text-node)))

(defun sexp-body ()
  (.prog1
   (.bind (.map 'list (.or (read-sexp)
                           (.is-not #'char= +sexp-close+)))
          (lambda (input)
            (let* ((pos (position #\space input))
                   (a (if pos (subseq input 0 pos) nil))
                   (x (if pos (subseq input pos) input)))
              (.identity `(,@(sexp-atom a) ,@x)))))
   (sexp-close)))

(defun read-delimiter (stream char)
  (declare (ignore stream char))
  (error "Unmatched bracket"))

(defun sexp-open ()
  (.is #'char= +sexp-open+))

(defun sexp-close ()
  (.is #'char= +sexp-close+))

(defun read-sexp ()
  (.progn
   (sexp-open)
   (sexp-body)))

;;;; interface --------------------------------------------------------

(defgeneric parse-scrawl (input &optional char)
  (:documentation "The parser interface")
  (:method ((input string) &optional char)
    (declare (ignore char))
    (parse (read-sexp)
           (subseq input 1)))
  (:method ((input stream) &optional char)
    (declare (ignore char))
    (parse (read-sexp) input)))

(defmacro enable-scrawl ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (push *readtable* *previous-readtables*)
    (setq *readtable* (copy-readtable))
    (set-macro-character +sexp-open+ 'parse-scrawl)
    (set-macro-character +sexp-close+ 'read-delimiter)))

(defmacro disable-scrawl ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (setq *readtable* (pop *previous-readtables*))))
