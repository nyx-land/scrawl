(defpackage #:scrawl.parser
  (:use :cl :scrawl)
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

(defun keypair (&optional key)
  (p:ap (lambda (x y)
          (cons (read-from-string x) y))
        (*> #'p:space
            (if key
                key
                (p:recognize
                 (<*> (p:char #\:)
                      (take-word)))))
        (*> #'p:space1
            (take-word))))

(defun reference ()
  (keypair (p:alt (p:string ":reference")
                  (p:string ":ref"))))

(defun metadata ()
  (p:many (p:alt (reference)
                 (keypair))))

(defun markup (type)
  (p:ap (lambda (meta)
          `(,type ,@meta))
        (p:opt (metadata))))

(defun section ()
  (p:ap (lambda (title meta children)
          `(section
            :title ,title
            ,@meta
            :children ,children))
        (p:take-until
         (p:alt (keypair)
                (blank-line)))
        (p:opt (metadata))
        (p:opt (*> (blank-line) (scrawl/body)))))

(defun inline ()
  (p:alt (scrawl (p:alt (p:char #\*)
                        (p:char #\/)
                        (p:char #\%)
                        (p:char #\_)
                        (p:char #\~)))
         (p:take-while
          (lambda (c)
            (not (charswitch c
                   #\[ #\]))))))

(defun paragraph ()
  (<* (p:ap (lambda (children)
              `(paragraph :children ,children))
            (inline))
      (blank-line)))

(defun scrawl/body ()
  (p:many (*> (p:not (p:alt (scrawl (p:char #\#))
                            (p:char #\])))
              (p:alt (scrawl)
                     (paragraph)))))

(defun scrawl/object (&optional atom)
  (lambda (offset)
    (bind (<* (if atom atom (take-word))
              #'p:multispace1)
        (offset result)
      (let ((a (if (zerop (length result))
                   (aref result 0)
                   (read-from-string result))))
        (case a
          ((or #\* #\/ #\% #\_ #\~)
           (scrawl/markup a))
          (#\# (section))
          (#\* (markup 'bold))
          (#\/ (markup 'italic))
          (#\% (markup 'code))
          (#\_ (markup 'underline))
          (#\~ (markup 'strikethrough))
          (#\$ (code-block))
          (#\> (block-quote))
          (#\< (inline-quote))
          (t (scrawl/key a)))))))

(defun scrawl (&optional atom)
  (p:between
   (p:char #\[)
   (p:alt (*> (p:char #\[)
              (scrawl))
          (if char (scrawl/object char)
              (scrawl/object)))
   (p:char #\])))

(defmacro charr (&rest chars)
  `(let* ((chars (sort (mapcar #'char-code ',chars) #'>))
          (out (make-array (1+ (car chars)) :initial-element nil)))
     (loop for c in chars
           do (setf (aref out c) t))
     out))

(defmacro charswitch (&rest chars)
  `(lambda (in)
     (or ,@(mapcar (lambda (c)
                     `(position ,c in))
                   chars))))

(defconstant +whitespace+
  (charr
   #\space #\tab #\return #\newline))

(defun any-of (chars)
  (lambda (c)
    (let ((cc (char-code c)))
      (and (< (length chars))
           (aref chars cc)))))

(defun not-of (chars)
  (lambda (c)
    (let ((cc (char-code c)))
      (or (> cc (length chars))
          (not (aref chars cc))))))

(defun make-syntax (&rest objects)
  (let ((out (make-hash-table)))
    (loop for x in objects
          as class = (car x)
          as constructors = (cdr x)
          do (mapcar (lambda (y)
                       (setf (gethash y out) class))
                     constructors))
    out))

(defvar *syntax*
  (make-syntax
    '(heading #\# :heading)))

(defun consume (chars)
  (lambda (in)
    (loop for c across in
          as i = 0 then (incf i)
          until (funcall (not-of chars) c)
          finally (return (subseq in i)))))

(defun ws-consume ()
  (consume +whitespace+))

(defun whitespace-p (c)
  (let ((cc (char-code c)))
    (and (< cc (length +whitespace+))
         (aref +whitespace+ cc))))

(defun ws-next (in)
  (funcall (charswitch
            #\space #\return
            #\newline #\tab)
           in))

(defun read-key (in)
  (let ((fc (aref in 0))
        (ws (ws-next in)))
    (when (and (char= fc #\:) ws)
      (let ((string (subseq in 1 ws)))
        (values
         (intern (string-upcase string)
                 :keyword)
         (funcall (ws-consume)
                  (subseq in (1+ (length string)))))))))

(defun read-class (a)
  (let ((fc (char a 0))
        (sc (char a 1)))
    (if (and fc (whitespace-p sc))
        (gethash fc *syntax*)
        (multiple-value-bind (key rest) (read-key a)
          (if key (values (gethash key *syntax*)
                          rest)
              nil)))))

(defun read-slots (in)
  ())


(defun sexp (in)
  (let* ((start (subseq in (1+ (position #\[ in))))
         (first-el (funcall (ws-consume) start))
         (class (read-class first-el)))
    (if class
        `(make-instance
          ',class
          ,@(funcall #'read-slots 
                     (consume-key in class))
          :body )
        )))

(defun parse (parser in)
  (funcall #'sexp in))


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
    (set-dispatch-macro-character #\# +sexp-open+
     #'(lambda (s c n)
         (declare (ignore n))
         (parse-scrawl s c)))
    (set-macro-character +sexp-close+ 'read-delimiter)))

(defmacro disable-scrawl ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (setq *readtable* (pop *previous-readtables*))))
