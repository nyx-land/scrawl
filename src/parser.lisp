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

(in-package :parcom)

;; TODO: this is such a stupid hack there's got to be a better way
(declaim (ftype (function (maybe-parse) always-parse) many-ls))
(defun many-ls (parser)
  "Parse 0 or more occurrences of a `parser'."
  (lambda (input)
    (declare (optimize (speed 3)))
    (labels ((recurse (acc in)
               (let ((res (funcall parser in)))
                 (etypecase res
                   (failure (ok in acc))
                   (parser (recurse (cons (parser-value res) acc)
                                    (parser-input res)))))))
      (fmap (lambda (x) (cons 'list x))
            (fmap #'nreverse (recurse '() input))))))

(in-package :scrawl.parser)

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
  `(alt ,@(mapcar (lambda (x)
                    `(<*> (read-tag ,(car x) ,(cadr x))
                          ,@(if (cddr x)
                                (cddr x)
                                '((sexp-bd)))))
                  body)
        (string-take)))

(defmacro cswitch (&body body)
  `(or ,@(mapcar (lambda (c) `(char= c ,c))
                 body)))

(defun whitespace-p (c)
  (cswitch #\space #\tab #\newline #\return))

;;;; parsers ----------------------------------------------------------

(defun ok (input)
  (<$ input (parcom:take 0)))

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

(defun string-take ()
  (<*> (ok 'make-text)
       (<*> (ok 'string-out)
            (*> (peek (anybut +sexp-close+))
                (take-while (lambda (c)
                              (not (cswitch #\[ #\] #\newline))))))))

(defun sexp-bd ()
  (parcom::many-ls (alt (sexp-read)
                        (string-take))))

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
    (:code (char #\~))
    (:underline (char #\_))
    (:strikethrough (char #\+))
    ;; (:section
    ;;  (char #\#)
    ;;  (read-title))
    ;; (:web-link
    ;;  (char #\@)
    ;;  (word))
    ;; (:code-block
    ;;  (char #\$)
    ;;  (word))
    ;; (:unordered-list
    ;;  (char #\-)
    ;;  (read-unordered-list))
    ;; (:ordered-list
    ;;  (char #\=)
    ;;  (read-ordered-list))
    )
  )

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
