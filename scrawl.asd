(defsystem "scrawl"
  :depends-on ("common-doc" "smug")
  :components ((:module "src"
                :components
                ((:file "parser")
                 (:file "emitter")
                 (:file "scrawl")))))
