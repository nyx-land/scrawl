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
   :consume
   :fmap
   :take
   :skip)
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
   :make-paragraph
   :make-meta
   :make-bold
   :make-italic
   :make-underline
   :make-strikethrough
   :make-subscript
   :make-superscript
   :make-code
   :make-code-block
   :make-block-quote
   :make-inline-quote
   :make-text
   :make-web-link
   :make-section
   :make-unordered-list
   :make-ordered-list
   :make-definition-list
   :make-list-item
   :make-definition
   :make-document
   :make-table
   :make-row
   :make-cell
   :make-image
   :make-figure))

(in-package :scrawl.parser)

(defconstant +sexp-open+ #\[)
(defconstant +sexp-close+ #\])

(defparameter *whitespace*
  '(#\Space #\Tab #\Newline #\Return))

(defvar *previous-readtables* nil)

;;(defparameter *ipsum* (lorem-ipsum:paragraphs 5))

;;;; utils ------------------------------------------------------------

(defun read-string (stream)
  (let ((out (make-string-output-stream)))
    (loop for line = (read-line stream nil :eof)
          until (eq :eof line)
          do (write-sequence line out))
    (get-output-stream-string out)))

(defun string-out (input)
  (with-output-to-string (out nil :element-type 'base-char)
    (write-sequence input out)))

(defun parse-debug (parser input)
  (funcall parser (parcom:in input)))

(defun emit-debug (object)
  (common-doc.format:emit-to-string
   (make-instance 'common-html:html)
   object))

(defmacro make-tags (&body body)
  `(alt (*> (alt (peek (char #\())
                 (parcom:string ":lisp"))
            (read-lisp))
        ,@(mapcar (lambda (x)
                    `(<*> (read-tag ,(car x) ,(cadr x))
                          ,@(if (cddr x)
                                (cddr x)
                                '((sexp-bd)))))
                  body)
        (sexp-bd)))

(defmacro cswitch (&body body)
  `(or ,@(mapcar (lambda (c) `(char= c ,c))
                 body)))

(defun whitespace-p (c)
  (cswitch #\space #\tab #\newline #\return))

;;;; parsers ----------------------------------------------------------

(defun ok (input)
  (<$ input (parcom:take 0)))

(defun many-x (parser x)
  (lambda (input)
    (fmap (lambda (y)
            (cons x y))
          (funcall (many parser) input))))

(defun word ()
  (take-while
   (lambda (c)
     (not (whitespace-p c)))))

(defun take-sexp ()
  (*> (peek (anybut +sexp-close+))
      (take-while (lambda (c)
                    (not (cswitch #\[ #\] #\newline))))))

(defun string-take ()
  (<*> (ok 'string-out)
       (take-sexp)))

(defun read-lisp ()
  (lambda (input)
    (fmap #'read-from-string
          (funcall (take-sexp) input))))

(defun read-text ()
  (<*> (ok 'make-text)
       (string-take)))

(defun parse-key ()
  ())

(defun read-pairs ()
  (<*> (ok 'list)
       ()))

(defun read-title ()
  (<* (*> (peek (anybut +sexp-close+))
          (take-while (lambda (c)
                        (not (cswitch #\[ #\] #\newline)))))
      (consume #'whitespace-p)))

(defun read-list ()
  (many-x (<*> (ok 'make-list-item)
               (alt (sexp-read)
                    (read-text)))
          'list))

(defun read-definitions ())
(defun read-table ())

(defun sexp-bd ()
  (many-x (alt (sexp-read)
               (read-text))
          'list))

(defun read-tag (name c)
  (<* (<$ (read-from-string
           (format nil "make-~(~a~)" name))
          (alt c (parcom:string
                  (format nil "~s" name))))
      (consume #'whitespace-p)))

(defun sexp-tags ()
  (make-tags
    (:italic (char #\/))
    (:bold (char #\*))
    (:code (char #\%))
    (:underline (char #\_))
    (:strikethrough (char #\+))
    (:superscript (parcom:string ":sup"))
    (:subscript (parcom:string ":sub"))
    (:inline-quote (char #\>))
    (:block-quote (char #\<))
    (:section
     (char #\#)
     (read-title))
    (:web-link
     (char #\@)
     (word))
    (:code-block
     (char #\$)
     (word))
    (:unordered-list
     (char #\-)
     (read-list))
    (:ordered-list
     (char #\=)
     (read-list))
    (:definition
     (char #\~)
     (read-definitions))
    (:table
     (parcom:string ":tab")
     (read-table))
    (:image
     (parcom:string ":img")
     (word)
     (ok :description)
     (string-take))
    (:figure
     (parcom:string ":fig")
     (<*> (ok 'make-image)
          (word))
     (read-text))
    ;; (:meta
    ;;  (parse-key)
    ;;  (read-pairs))
    ))

(defun sexp-read ()
  (between
   (char +sexp-open+)
   (sexp-tags)
   (char +sexp-close+)))

;;;; interface --------------------------------------------------------

(defgeneric parse-scrawl (input &optional char)
  (:documentation "The parser interface")
  (:method ((input string) &optional char)
    (declare (ignore char))
    (parse (sexp-read) input))
  (:method ((input stream) &optional char)
    (declare (ignore char))
    (unread-char #\[ input)
    (let ((string (read-string input)))
      (parse (sexp-read) string))))

(defmacro enable-scrawl ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (push *readtable* *previous-readtables*)
    (setq *readtable* (copy-readtable))
    (set-macro-character +sexp-open+ 'parse-scrawl)
    (set-macro-character +sexp-close+ 'read-delimiter)))

(defmacro disable-scrawl ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (setq *readtable* (pop *previous-readtables*))))
