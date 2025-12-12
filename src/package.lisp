(defpackage #:scrawl
  (:use :cl)
  (:export
   :format* :bind :charswitch
   :take-line :blank-line
   :whitespace-p :ws
   :take-word
   :trim-ws :parse-debug))

(in-package :scrawl)
