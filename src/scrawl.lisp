(defpackage #:scrawl
  (:use :cl)
  (:import-from :common-doc.format
                :document-format
                :parse-document
                :emit-document))

(in-package :scrawl)

(defclass scrawl (document-format) ()
  (:documentation "The Scrawl format"))

(defmethod parse-document ((document-format scrawl)
                           (input string))
  (declare (ignore document-format))
  (scrawl.parser:parse input))

(defmethod parse-document ((document-format scrawl)
                           (input pathname))
  (declare (ignore document-format))
  (scrawl.parser:parse (uiop:read-file-string input)))

(defmethod emit-document ((document-format scrawl)
                          (document common-doc:document-node)
                          stream)
  (declare (ignore document-format))
  (scrawl.emitter:emit node stream))

(defmethod emit-document ((document-format scrawl)
                          (document common-doc:document)
                          stream)
  (declare (ignore document-format))
  (scrawl.emitter:emit document stream))
