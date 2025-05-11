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
   :any-but
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
   :skip
   :opt)
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
  (with-output-to-string (out nil)
    (write-sequence input out)))

(defun parse-debug (parser input)
  (funcall parser (parcom:in input)))

(defun emit-debug (object)
  (common-doc.format:emit-to-string
   (make-instance 'common-html:html)
   object))

(defmacro cswitch (&body chars)
  `(or ,@(mapcar (lambda (c) `(char= c ,c))
                 chars)))

(defun whitespace-p (c)
  (cswitch #\space #\tab #\return #\newline))

;;;; parsers ----------------------------------------------------------

(defun flatten (tree)
  (cond ((null tree) nil)
        ((consp (car tree))
         (concatenate 'list
                      (car tree)
                      (flatten (cdr tree))))
        (t (cons (car tree)
                 (flatten (cdr tree))))))

(defun interleave (parser)
  (lambda (input)
    (fmap #'flatten
          (funcall parser input))))

(defun ok (input)
  (<$ input (parcom:take 0)))

(defun many-x (parser x)
  (lambda (input)
    (fmap (lambda (y)
            (cons x y))
          (funcall (many parser) input))))

;; TODO: inserting `FORMAT' is hacky
(defun word ()
  (alt (between (char #\")
                (take-while
                 (lambda (c)
                   (not (cswitch #\"))))
                (char #\"))
       (take-while
        (lambda (c)
          (not (cswitch #\[ #\]
                 #\space #\return
                 #\tab #\newline))))))

(defun take-sexp ()
  (*> (peek (any-but +sexp-close+))
      (take-while (lambda (c)
                    (not (cswitch #\[ #\]))))))

(defun string-take ()
  (<*> (ok 'string-out)
       (take-sexp)))

(defun read-text ()
  (<*> (ok 'make-text)
       (string-take)))

(defun read-paragraph ()
  (*> (parcom:string
       (string-out (format nil "~%~%")))
      (<*> (ok 'make-paragraph)
           (read-text))))

(defun read-lisp ()
  (lambda (input)
    (fmap #'read-from-string
          (funcall (take-sexp) input))))

(defun read-reference ()
  (*> (parcom:string "#:")
      (word)))

(defun parse-keypair ()
  (*> (peek (any-but +sexp-close+))
      (<*> (ok 'cons)
           (<* (word)
               (consume #'whitespace-p))
           (<* (word)
               (consume #'whitespace-p)))))

(defun read-pairs ()
  (<*> (ok 'make-meta)
       (many-x (parse-keypair)
               'list)))

(defun read-title ()
  (<* (*> (*> (peek (any-but +sexp-close+))
              (peek (any-but +sexp-open+))
              (peek (any-but #\newline)))
          (<*> (ok 'make-text)
               (<*> (ok 'string-out)
                    (take-while
                     (lambda (c)
                       (not (cswitch #\[ #\] #\newline)))))))
      (consume #'whitespace-p)))

(defun read-list ()
  (many-x (<*> (ok 'make-list-item)
               (alt (sexp-read)
                    (read-text)))
          'list))

(defun read-definitions ())
(defun read-table ())

;;;; ------------------------------------------------------------------
;;;; tag parsers ------------------------------------------------------

(defun read-tag (name alt-tag)
  (if alt-tag
      (alt (if (keywordp alt-tag)
               (parcom:string
                (string-out
                 (format nil "~(~s~)" alt-tag)))
               alt-tag)
           (parcom:string
            (string-out
             (format nil "~s" name))))
      (opt
       (parcom:string
        (string-out
         (format nil "~s" name))))))

(defmacro deftag (name alt-tag node-p &body body)
  `(*> (read-tag ,name ,alt-tag)
       (consume #'whitespace-p)
       ,(if node-p
            `(<*> (ok 'apply)
                  (ok '(function make-instance))
                  (ok ',(read-from-string
                         (format nil "'~a" name)))
                  ,@body)
            `(<*> (ok ',name)
                  ,@body))))

(defmacro with-args (&body body)
  `(alt ,@(mapcar
           (lambda (x)
             (cond ((eq (car x) '&key)
                    `(many-x
                      (with-args ,@(cdr x))
                      'list))
                   ((keywordp (car x))
                    `(sexp-read
                      (deftag ,(car x) ,(cadr x) nil
                        ,@(cddr x))))
                   (t `(deftag ,(intern
                                 (symbol-name (car x))
                                 :keyword)
                           nil nil
                         ,@(cddr x)))))
           body)))

(defmacro with-nodes (&body body)
  `(alt ,@(mapcar
           (lambda (x)
             (destructuring-bind
                 (name alt-tag &rest node-body) x
               `(sexp-read
                 (deftag ,name ,alt-tag t
                   (with-args
                     ,@node-body)))))
           body)))

(defun sexp-bd ()
  (many-x (alt (sexp-read)
               (read-paragraph)
               (read-text))
          'list))

;; TODO: this protocol for parsing different tags is very hacky
;; needs to have a more extensible way of switching between
;; tables depending on the context
;; (defun sexp-args ()
;;   (with-args
;;     (:metadata
;;      (parcom:string ":meta")
;;      (read-pairs))
;;     (:reference
;;      (parcom:string ":ref")
;;      (word))))

;; (defun sexp-tags ()
;;   (make-tags
;;     (:paragraph (parcom:string (format nil "~%~%")))
;;     (:italic (char #\/))
;;     (:bold (char #\*))
;;     (:code (char #\%))
;;     (:underline (char #\_))
;;     (:strikethrough (char #\+))
;;     (:superscript (parcom:string ":sup"))
;;     (:subscript (parcom:string ":sub"))
;;     (:inline-quote (char #\>))
;;     (:block-quote (char #\<))
;;     (:section
;;      (char #\#)
;;      (:title
;;       (parcom:string ":title")
;;       (read-title)))
;;     (:web-link
;;      (char #\@)
;;      (word))
;;     (:code-block
;;      (char #\$)
;;      (word))
;;     (:unordered-list
;;      (char #\-)
;;      (read-list))
;;     (:ordered-list
;;      (char #\=)
;;      (read-list))
;;     (:definition
;;      (char #\~)
;;      (read-definitions))
;;     (:table
;;      (parcom:string ":tab")
;;      (read-table))
;;     (:image
;;      (parcom:string ":img")
;;      (word)
;;      (ok :description)
;;      (string-take))
;;     (:figure
;;      (parcom:string ":fig")
;;      (<*> (ok 'make-image)
;;           (word))
;;      (read-text))
;;     ;; (:meta
;;     ;;  (parse-key)
;;     ;;  (read-pairs))
;;     ))

(defun sexp-read (parser)
  (<* (*> (opt (consume #'whitespace-p))
          (between
           (char +sexp-open+)
           (*>
            (consume #'whitespace-p)
            (<* parser (consume #'whitespace-p)))
           (char +sexp-close+)))
      (opt (consume #'whitespace-p))))

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
