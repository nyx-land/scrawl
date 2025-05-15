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

(defun ok (input)
  (<$ input (parcom:take 0)))

(defun many-x (parser x)
  (lambda (input)
    (fmap (lambda (y)
            (cons x y))
          (funcall (many parser) input))))

;;;; body parsing -----------------------------------------------------

(defun read-text ()
  (<*> (p:pure 'make-text)
       (p:take-while
        (lambda (c)
          (not (cswitch #\[ #\]))))))

;;;; structure parsing ------------------------------------------------

(defun sexp-read (parser)
  (between
   (p:char #\[)
   (*>
    (opt (consume #'whitespace-p))
    (<* parser (consume #'whitespace-p)))
   (p:char #\])))

(defun name-key (name)
  (if (keywordp name)
      (p:string
       (format-parser "~(~s~)" name))
      (p:string name)))

(defun make-tag (name tag)
  (if (characterp tag)
      (alt (name-key name)
           (p:char tag))
      (alt (name-key name)
           (name-key tag))))

(defun sexp-atom (name tag)
  (*> (if tag
          (make-tag name tag)
          (opt (p:string
                (format-parser "~s" name))))
      (consume #'whitespace-p)))

(defun make-arg (name tag parser &optional (sexp t))
  (let ((inner-parser (sexp-atom name tag))
        (parser-body (<*> (p:pure name)
                          parser)))
    (if sexp
        (sexp-read (*> inner-parser
                       parser-body))
        (*> inner-parser
            parser-body))))



(defmacro make-node (name alt-tag &body body)
  `(sexp-read
    (*> (sexp-atom ,name ,alt-tag)
        (<*> (p:pure 'apply)
             (p:pure '(function make-instance))
             (p:pure ',(read-from-string
                        (format nil "'~a" name)))
             (interleave
              (ls (interleave
                   ,@body)))))))

(defmacro with-args (parser &body body)
  `(,@parser ,@(mapcar
                (lambda (x)
                  `(make-arg
                    ,(if (keywordp (car x)) (car x)
                         (intern (car x) :keyword))
                    ,(cadr x)
                    ,@(cddr x)
                    ,(unless (keywordp (car x))
                       nil)))
                body)))

(defmacro with-nodes (&body body)
  `(many-x (alt ,@(mapcar
                   (lambda (x)
                     `(make-node ,(car x) ,(cadr x)
                        (with-args (<*>)
                          ,@(cddr x))))
                   body)
                (*> (peek (any-but #\]))
                    (peek (any-but #\[))
                    (read-text)))
           'list))

(defun scrawl-nodes ()
  ;; this is roughly what everything should be expanding to
  ;; and will be left here for reference temporarily
  ;;
  ;;
  ;; (many-x (alt (sexp-read (*> (sexp-atom :section #\#)
  ;;                             (<*> (p:pure 'apply)
  ;;                                  (p:pure '(function make-instance))
  ;;                                  (p:pure ''section)
  ;;                                  (scrawl-nodes))))
  ;;              (sexp-read (*> (sexp-atom :bold #\*)
  ;;                             (<*> (p:pure 'apply)
  ;;                                  (p:pure '(function make-instance))
  ;;                                  (p:pure ''bold)
  ;;                                  (scrawl-nodes))))
               
  ;;              (*> (peek (any-but #\]))
  ;;                  (peek (any-but #\[))
  ;;                  (read-text)))
  ;;         'list)
  (with-nodes
    (:section
        #\#
      (:children
       nil
       (scrawl-nodes)))
    (:bold
     #\*
     (:children
      nil
      (scrawl-nodes)))))
