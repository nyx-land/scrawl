(defsystem "scrawl"
  :depends-on ("common-doc" "parcom" "alexandria" "closer-mop" "common-html")
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "util")
                 (:file "classes")
                 (:file "parser")
                 (:file "emitter")
                 (:file "scrawl")))))
