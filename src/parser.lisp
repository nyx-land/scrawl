(defpackage #:scrawl.parser
  (:use :cl)
  (:local-nicknames (:p :parcom))
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

(defun format-parser (string &rest args)
  (with-output-to-string (stream)
    (apply #'format stream string args)))

(defmacro cswitch (&body chars)
  `(or ,@(mapcar (lambda (c) `(char= c ,c))
                 chars)))

(defun whitespace-p (c)
  (cswitch #\space #\tab #\return #\newline))

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

(defun remnil (parser)
  (lambda (input)
    (fmap (lambda (x) (remove nil x))
          (funcall parser input))))

(defun many-x (parser x)
  (lambda (input)
    (fmap (lambda (y)
            (cons x y))
          (funcall (many parser) input))))

(defun take-string ()
  (*> (peek (any-but #\]))
      (p:take-until
       (alt (p:char #\[)
            (p:char #\])
            (p:char #\newline)))))

(defun word ()
  (p:take-until
   (alt #'p:space
        (p:char #\[)
        (p:char #\]))))

(defun read-key (name)
  (<$ name (*> (p:char #\:)
               (alt (p:string
                     (format-parser "~(~a~)" name))
                    (p:string
                     (format-parser "~a" name))))))

(defun sexp-atom (input)
  (etypecase input
    (keyword (read-key input))
    (character (p:char input))
    (string (p:string input))
    (null (p:pure t))))

(defun read-expr (name tag parser &key node-p sexp)
  (let ((expr (*> (if tag
                      (alt (sexp-atom name)
                           (sexp-atom tag))
                      (sexp-atom nil))
                  (consume #'whitespace-p)
                  (if node-p
                      (<*> (p:pure 'apply)
                           (p:pure '(function make-instance))
                           (p:pure (read-from-string
                                    (format-parser "'~a" name)))
                           parser)
                      (<*> (p:pure name)
                           parser)))))
    (if sexp (sexp-read expr) expr)))

(defmacro arg (name tag sexp &body parser)
  `(read-expr ,name ,tag ,@parser :node-p nil :sexp ,sexp))

(defmacro with-args (&body body)
  `(remnil (interleave (<*> (p:pure 'list) ,@body))))

(defun default-args ()
  (p:count 2 (opt (alt (arg :metadata :meta t
                         (take-string))
                       (arg :reference :ref t
                         (word))))))

(defun recur ()
  (opt (arg :children nil nil
         (many-x (scrawl) 'list))))

(defmacro node (name tag &body parser)
  `(let ((&def (default-args))
         (&rec (recur)))
     (read-expr ,name ,tag
                (with-args ,@parser)
                :node-p t :sexp t)))

(defun scrawl ()
  (alt (node :section #\#
         (arg :title nil nil
           (take-string))
         &def &rec)
       (node :bold #\*
         &rec &def)
       (node :italic #\/
         &rec &def)
       (read-expr :text-node nil
                  (with-args
                    (arg :text nil nil
                      (take-string)))
                  :node-p t :sexp nil)))


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
