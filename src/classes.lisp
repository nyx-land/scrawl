(in-package :scrawl)

(defvar *syntax-table* (make-hash-table))

(defclass scrawl-meta (standard-class)
  ((shorthand :initarg :shorthand
              :initform nil
              :accessor shorthand)
   (parser :initarg :parser
           :initform nil
           :accessor parser)))

(defmethod c2mop:validate-superclass
    ((class scrawl-meta) (superclass standard-class))
  t)

(defclass positional-direct-slot-definition
    (c2mop:standard-direct-slot-definition)
  ((positional
    :initarg :positional
    :initform nil
    :documentation "If this slot is `T' it will be parsed as a positional argument for the
construction of a node within a scrawl expression.")))

(defmethod c2mop:direct-slot-definition-class
    ((class scrawl-meta) &key positional &allow-other-keys)
  (if positional 'positional-direct-slot-definition
      (call-next-method)))

(defclass positional-effective-slot-definition
    (c2mop:standard-effective-slot-definition)
  ((positional
    :initarg :positional
    :initform nil)))

(defmethod c2mop:effective-slot-definition-class
    ((class scrawl-meta) &rest initargs)
  'positional-effective-slot-definition)

(defmethod c2mop:compute-effective-slot-definition
    ((class scrawl-meta) name direct-slots)
  (let ((effective-slot (call-next-method)))
    (when (some (lambda (ds)
                  (typep ds 'positional-direct-slot-definition))
                direct-slots)
      (setf (slot-value effective-slot 'positional) t))
    effective-slot))

(defun ensure-constructor (class)
  (c2mop:ensure-finalized class)
  (let ((slots (c2mop:class-slots class))
        (lambda-list (loop for slot in slots
                           if (slot-value slot 'positional)
                             collect slot into posits
                           else
                             collect slot into keys
                           finally (return `(,@posits &key ,@keys)))))
    ;; TODO: we need a way of determining what to parse based off
    ;; whether or not a slot is positional or a keyword. positional
    ;; slots will need to get parsed first in the order they're
    ;; defined
    (setf (slot-value class 'parser)
          (compile nil `(lambda
                          ,lambda-list
                          ,(alexandria:flatten
                            (mapcar (lambda (x)
                                      (list (intern (symbol-name x))
                                            x))
                                    (remove '&key lambda-list))))))))

(defun ensure-node (class shorthand)
  (let ((name (intern (symbol-name (class-name class)) :keyword)))
    (setf (gethash name *syntax-table*)
          class)
    (when shorthand
      (mapcar (lambda (s)
                (setf (gethash s *syntax-table*)
                      class))
              shorthand))
    (ensure-constructor class)))

(defmethod initialize-instance
    :after ((class scrawl-meta)
            &key shorthand &allow-other-keys)
  (unless (eq (car shorthand) 'non-node)
    (ensure-node class shorthand))
  class)

(defmethod reinitialize-instance
    :around ((class scrawl-meta)
             &key shorthand &allow-other-keys)
  (unless (eq (car shorthand) 'non-node)
    (ensure-node class shorthand))
  (call-next-method))

(defclass header ()
  ((title :initarg :title :accessor title
          :positional t)
   (tags :initarg :tags :accessor tags))
  (:shorthand #\#)
  (:metaclass scrawl-meta))
