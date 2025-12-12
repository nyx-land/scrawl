(in-package :scrawl)

(defun make-simple-array (string)
  (let ((out (make-string-output-stream)))
    (loop for c across string
          do (write-char c out))
    (get-output-stream-string out)))

;; for some reason we can't return a FORMAT string with the
;; (simple-array character) type parcom wants :^)
(defun format* (control-string &rest format-arguments)
  (make-simple-array
   (apply #'format nil control-string format-arguments)))

(defmacro bind (parser (offset result)
                &body body)
  "Bind parser results to `RES' and `NEXT' implicitly. Expects to be
called within a lexical scope like a custom parser function that has
`OFFSET' bound."
  `(multiple-value-bind (,result next)
       (funcall ,parser ,offset)
     (if (p:failure? ,result)
         (p:fail next)
         (funcall ,@body next))))

(defmacro charswitch (var &body chars)
  `(or ,@(mapcar (lambda (x)
                   `(eq ,var ,x))
                 chars)))

(defun take-line ()
  (lambda (offset)
    (multiple-value-bind (result next)
        (funcall
         (p:take-while (lambda (c) (not (eq c #\newline))))
         offset)
      (if (or (p:empty? result)
              (p:failure? result))
          (p:fail next)
          (values result next)))))

(defun blank-line ()
  (p:between
   (p:char #\newline)
   (p:opt (*> (p:take-while
               (lambda (c)
                 (or (eq c #\space)
                     (eq c #\tab)
                     (eq c #\return))))
              (<*> (p:pure 'blank-line))))
   (p:sneak #\newline)))

(defun take-word ()
  (p:take-while
   (lambda (c) (not (or (whitespace-p c)
                        (eq c #\[)
                        (eq c #\]))))))

(defun whitespace-p (c)
  (charswitch c
    #\newline #\space #\return #\tab))

(defun ws ()
  (p:any-if
   (lambda (c)
     (whitespace-p c))))

(defun trim-ws (parser)
  (<* parser (p:consume #'whitespace-p)))

(defun parse-debug (parser input)
  (funcall parser (p:in input)))


