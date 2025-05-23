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
   :subscript
   :superscript
   :strikethrough
   :code
   :code-block
   :web-link
   :block-quote
   :inline-quote
   ;; constructors
   :make-meta
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

;;;; utilities --------------------------------------------------------

(defun read-into-string (stream &optional (out (make-string-output-stream)))
  (loop for c = (read-char stream t nil t)
        until (char= c #\])
        do (if (char= c #\[)
               (progn (write-char #\[ out)
                      (read-into-string stream out))
               (write-char c out))
        finally (write-char #\] out))
  out)

(defun format-parser (string &rest args)
  (with-output-to-string (stream)
    (apply #'format stream string args)))

(defmacro cswitch (&body chars)
  `(or ,@(mapcar (lambda (c) `(char= c ,c))
                 chars)))

(defun whitespace-n ()
  (alt (p:char #\space)
       (p:char #\tab)
       (p:char #\return)
       (p:char #\newline)))

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

(defun count-x (n parser x)
  (lambda (input)
    (fmap (lambda (y)
            (cons x y))
          (funcall (p:count n parser) input))))

(defun take-string ()
  (<* (*> (peek (any-but #\]))
          (peek (any-but #\newline))
          (p:take-until
           (alt (*> (opt #'p:space)
                    (p:char #\[))
                (p:char #\])
                (*> #'p:newline #'p:newline))))
      #'p:space))

(defun word ()
  (p:take-until
   (alt (peek (whitespace-n))
        (p:char #\[)
        (p:char #\]))))

;;;; nodes & args -----------------------------------------------------

(defun take-text ()
  (<*> (p:pure 'make-text)
       (take-string)))



(defun read-pair ()
  (*> (peek (any-but #\[))
      (peek (any-but #\]))
      (count-x 2 (<* (alt (between (p:char #\")
                                   (take-string)
                                   (p:char #\"))
                          (between (p:char #\()
                                   ()
                                   (p:char #\)))
                          (word))
                     (consume #'whitespace-p))
               'cons)))

(defun read-key (name)
  (<$ name (*> (p:char #\:)
               (alt (p:string
                     (format-parser "~(~a~)" name))
                    (p:string
                     (format-parser "~a" name))))))

(defun sexp-atom (input)
  (etypecase input
    (function input)
    (keyword (read-key input))
    (character (p:char input))
    (string (p:string input))
    (null (p:pure t))))

(defun sexp-read (parser)
  (between
   (p:char #\[)
   (*>
    (opt #'p:space)
    parser)
   (p:char #\])))

(defun read-expr (name tag parser &key node-p sexp)
  (let ((expr (*> (if tag
                      (alt (sexp-atom name)
                           (sexp-atom tag))
                      (sexp-atom nil))
                  #'p:space
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
  `(<* (read-expr ,name ,tag ,@parser :node-p nil :sexp ,sexp)
       #'p:space))

(defmacro with-args (&body body)
  `(remnil (interleave (<*> (p:pure 'list) ,@body))))

(defun default-args ()
  (p:count 2 (opt (alt (arg :metadata :meta t
                         (<*> (p:pure 'make-meta)
                              (many-x (read-pair) 'list)))
                       (arg :reference :ref t
                         (word))))))

(defun recur ()
  (opt (arg :children nil nil
         (many-x (scrawl) 'list))))

(defmacro node (name tag &body parser)
  `(let ((&def (interleave (default-args)))
         (&rec (recur)))
     (read-expr ,name ,tag
                (with-args ,@parser)
                :node-p t :sexp t)))

;; TODO: reference and meta args aren't being parsed correctly and
;; are read as being part of the last text node
(defun paragraph ()
  (read-expr :paragraph (*> #'p:newline)
             (with-args
               (arg :children nil nil
                 (many-x (*> (*> (opt #'p:newline)
                                 (peek (any-but #\newline)))
                             (scrawl))
                         'list))
               (interleave (default-args)))
             :node-p t :sexp nil))

(defun text ()
  (read-expr :text-node (opt #'p:newline)
             (with-args
               (arg :text nil nil
                 (take-string))
               (interleave (default-args)))
             :node-p t :sexp nil))

(defun scrawl ()
  (alt (node :section #\#
         (arg :title nil nil
           (take-text))
         &def &rec)
       (node :subscript :sub
         &def &rec)
       (node :superscript :sup
         &def &rec)
       (node :image :img
         &def &rec)
       (node :figure :fig
         &def &rec)
       (node :bold #\*
         &def &rec)
       (node :italic #\/
         &def &rec)
       (node :code #\%
         &def &rec)
       (node :code-block #\$
         &def &rec)
       (node :block-quote #\>
         &def &rec)
       (node :inline-quote #\<
         &def &rec)
       (node :underline #\_
         &def &rec)
       (node :strikethrough #\~
         &def &rec)
       (paragraph)
       (text)))


;;;; interface --------------------------------------------------------

(defgeneric parse-scrawl (input &optional char)
  (:documentation "The parser interface")
  (:method ((input string) &optional char)
    (declare (ignore char))
    (parse (scrawl) input))
  (:method ((input stream) &optional char)
    (declare (ignore char))
    (let ((out (make-string-output-stream)))
      (write-char #\[ out)
      (parse (scrawl)
             (get-output-stream-string
              (read-into-string input out))))))

(defvar *previous-readtables* nil)

(defconstant +sexp-open+ #\[)
(defconstant +sexp-close+ #\])

(defun read-delimiter (stream char)
  (declare (ignore stream))
  (error "Delimiter ~S shouldn't be read alone" char))

(defmacro enable-scrawl ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (push *readtable* *previous-readtables*)
    (setq *readtable* (copy-readtable))
    (set-macro-character +sexp-open+ 'parse-scrawl)
    (set-macro-character +sexp-close+ 'read-delimiter)))

(defmacro disable-scrawl ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (setq *readtable* (pop *previous-readtables*))))
