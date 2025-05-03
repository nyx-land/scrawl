(defsystem "scrawl"
  :depends-on ("common-doc" "parcom" "alexandria" "common-html")
  :components ((:module "src"
                :components
                ((:file "parser")
                 (:file "emitter")
                 (:file "scrawl")))))
