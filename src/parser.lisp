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
   :take-while
   :consume)
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
   :children
   :bold
   :italic
   :underline
   :strikethrough
   :code
   :code-block
   :web-link
   ;; constructors
   :make-bold
   :make-italic
   :make-underline
   :make-strikethrough
   :make-code
   :make-code-block
   :make-text
   :make-web-link
   :make-section))

(in-package :scrawl.parser)

(defconstant +sexp-open+ #\[)
(defconstant +sexp-close+ #\])

(defparameter *whitespace*
  '(#\Space #\Tab #\Newline #\Return))

(defvar *previous-readtables* nil)

;;(defparameter *ipsum* (lorem-ipsum:paragraphs 5))

;;;; utils ------------------------------------------------------------

(defun sexp-end (c)
  (char= c +sexp-close+))

(defun sexp-start (c)
  (char= c +sexp-open+))

(defun parse-debug (parser input)
  (funcall parser (parcom:in input)))

(defun emit-debug (object)
  (common-doc.format:emit-to-string
   (make-instance 'common-html:html)
   object))

(defmacro scrawl-tags (&body tags)
  `(alt ,@(mapcar (lambda (x)
                    `(<*> (<* (<$ ',(read-from-string
                                     (format nil "make-~a"
                                             (symbol-name (car x))))
                                  (alt ,(cadr x)
                                       (parcom:string ,(format nil "~s" (car x)))))
                              (consume #'whitespace-p))
                          ,@(append (cddr x) '((sexp-body)))))
                  tags)
        (sexp-body)))

(defmacro cswitch (&body body)
  `(or ,@(mapcar (lambda (c) `(char= c ,c))
                 body)))

(defun whitespace-p (c)
  (cswitch #\space #\tab #\newline #\return))

;;;; parsers ----------------------------------------------------------

(defun ok (input)
  (<$ input (parcom:take 0)))

(defun take-string ()
  (<*> (ok 'make-text)
       (*> (peek (anybut +sexp-close+))
           (take-while (lambda (c)
                         (not (cswitch #\[ #\])))))))

(defun word ()
  (take-while
   (lambda (c)
     (not (whitespace-p c)))))

(defun read-title ()
  (<* (*> (peek (anybut +sexp-close+))
          (take-while (lambda (c)
                        (not (cswitch #\[ #\] #\newline)))))
      (consume #'whitespace-p)))

(defun read-unordered-list ())
(defun read-ordered-list ())

;; TODO: need to not have the lists be nested
(defun read-tags ()
  (scrawl-tags
    (:section
     (char #\#)
     (read-title))
    (:italic (char #\/))
    (:bold (char #\*))
    (:web-link
     (char #\@)
     (word))
    (:code (char #\~))
    (:code-block
     (char #\$)
     (word))
    ;; (:unordered-list
    ;;  (char #\-)
    ;;  (read-unordered-list))
    ;; (:ordered-list
    ;;  (char #\=)
    ;;  (read-ordered-list))
    (:underline (char #\_))
    (:strikethrough (char #\+))))

(defun sexp-body ()
  (many (alt (read-sexp)
             (take-string))))

(defun sexp-meta ()
  (<*> (read-tags)
       (sexp-keys)
       (sexp-body)))

(defun read-sexp ()
  (between
   (char +sexp-open+)
   (read-tags)
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
