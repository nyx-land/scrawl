(defsystem "scrawl"
  :depends-on ("common-doc" "smug" "alexandria")
  :components ((:module "src"
                :components
                ((:file "parser")
                 (:file "emitter")
                 (:file "scrawl")))))
