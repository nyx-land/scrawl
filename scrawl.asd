(defsystem "scrawl"
  :depends-on ("common-doc" "parcom" "alexandria")
  :components ((:module "src"
                :components
                ((:file "parser")
                 (:file "emitter")
                 (:file "scrawl")))))
