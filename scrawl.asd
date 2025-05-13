(defsystem "scrawl"
  :depends-on ("common-doc" "parcom" "alexandria" "common-html")
  :components ((:module "src"
                :components
                ((:file "parser_refactor")
                 (:file "emitter")
                 (:file "scrawl")))))
