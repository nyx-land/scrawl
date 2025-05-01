(defpackage #:scrawl.parser
  (:use :cl)
  (:shadowing-import-from
   :parcom
   :char)
  (:import-from
   :parcom
   :alt
   :peek
   :any
   :anybut
   :many
   :<$
   :*>
   :<*
   :<*>
   :between
   :parse
   :take-while)
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
   :paragraph
   :children))

(in-package :scrawl.parser)

(defconstant +sexp-open+ #\[)
(defconstant +sexp-close+ #\])

(defparameter *whitespace*
  '(#\Space #\Tab #\Newline #\Return))

(defvar *previous-readtables* nil)

;;(defparameter *ipsum* (lorem-ipsum:paragraphs 5))

;;;; utils ------------------------------------------------------------

(defun whitespace-p (c)
  (member c *whitespace*))

(defun sexp-end (c)
  (char= c +sexp-close+))

(defun sexp-start (c)
  (char= c +sexp-open+))

(defmacro scrawl-tags (&body tags)
  `(alt ,@(mapcar (lambda (x)
                    `(<* (<$ ,(car x)
                             (alt ,(cadr x)
                                  (parcom:string ,(format nil "~s" (car x)))))
                         (parcom:consume #'whitespace-p)))
                  tags)))

;;;; parsers ----------------------------------------------------------

(defun read-tags ()
  (scrawl-tags
    (:heading (char #\#))
    (:italics (char #\/))
    (:bold (char #\*))
    (:code (char #\~))
    (:link (char #\@))
    (:literal (char #\=))
    (:underline (char #\_))
    (:strikethrough (char #\+))))

(defun word ()
  (parcom:take-while
   (lambda (c)
     (not (whitespace-p c)))))

(defun key ()
  (<*> (*> (parcom:peek (char #\:)) (word))
       (*> (parcom:consume #'whitespace-p)
           (word))))

(defun sexp-body ()
  (many (alt (read-sexp)
             (*> (parcom:peek (anybut +sexp-close+))
                 (take-while (lambda (c)
                               (not (or (char= c #\[)
                                        (char= c #\])))))))))

(defun read-sexp ()
  (between
   (char +sexp-open+)
   (sexp-body)
   (char +sexp-close+)))

;;;; interface --------------------------------------------------------

(defgeneric parse-scrawl (input &optional char)
  (:documentation "The parser interface")
  (:method ((input string) &optional char)
    (declare (ignore char))
    (parse (read-sexp) input))
  ;; (:method ((input stream) &optional char)
  ;;   (declare (ignore char))
  ;;   (parse (read-sexp) input))
  )

(defmacro enable-scrawl ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (push *readtable* *previous-readtables*)
    (setq *readtable* (copy-readtable))
    (set-macro-character +sexp-open+ 'parse-scrawl)
    (set-macro-character +sexp-close+ 'read-delimiter)))

(defmacro disable-scrawl ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (setq *readtable* (pop *previous-readtables*))))
