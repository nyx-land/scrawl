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

(defvar *previous-readtables* nil)
(defvar *tags*
  (alexandria:alist-hash-table
   '((#\# . 'section)
     (#\space . 'paragraph))))

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

(defun tag-p (c)
  (let ((tag (gethash c *tags*)))
    (if tag tag 'paragraph)))

(defun read-tag ()
  (.bind (.item)
         (lambda (input)
           (.identity (tag-p input)))))

(defun read-descend ()
  (.bind (.is #'char= +sexp-open+)
         (lambda (input)
           (declare (ignore input))
           (read-sexp)
           ;;(.identity input)
           )))

(defun read-end ()
  (.bind (.is #'char= +sexp-close+)
         (lambda (input)
           (declare (ignore input))
           (.fail))))

(defun read-cont ()
  (.bind (.is-not #'char= +sexp-close+)
         (lambda (input)
           (.identity input))))

(defun read-next ()
  (.map 'list (.or (read-descend)
                   (read-cont)
                   (read-end))))

(defun read-delimiter (stream char)
  (declare (ignore stream char))
  (error "Unmatched bracket"))

(defun read-sexp ()
  (.let* ((tag-x (read-tag))
          (body (read-next)))
    (.identity (list tag-x body))))

(defun read-scrawl (stream char)
  (declare (ignore char))
  (parse (read-sexp) stream))

(defmacro enable-scrawl ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (push *readtable* *previous-readtables*)
    (setq *readtable* (copy-readtable))
    (set-macro-character +sexp-open+ 'read-scrawl)
    (set-macro-character +sexp-close+ 'read-delimiter)))

(defmacro disable-scrawl ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (setq *readtable* (pop *previous-readtables*))))
