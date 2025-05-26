# Scrawl

To enable formatting in Emacs, add the following to your `.init.el`:

```emacs-lisp

;; if using SLIME
(setq lisp-indent-function 'common-lisp-indent-function)

;; if using Sly
(setq lisp-indent-function 'sly-common-lisp-indent-function)

(modify-syntax-entry ?\[ "(]" lisp-mode-syntax-table)
(modify-syntax-entry ?\] ")[" lisp-mode-syntax-table)

```

Now everything will be indented nicely and is read as s-expressions
the same as any other lisp code, which allows for Scrawl to be
seamlessly integrated into a structural editing workflow.