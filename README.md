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

Now everything will be indented nicely[^1] and is read as s-expressions
the same as any other lisp code, which allows for Scrawl to be
seamlessly integrated into a structural editing workflow.

---
1. Note on formatting: As of right now I am not aware of a way to
fix the following indentation issue in Emacs:

```
[# section

    body text goes here]

[# section with more than one word

  body text goes here]
```

As you can see this indents differently if there's only one word on a
line, and I am not going to write an entire Emacs mode for something
that is supposed to integrate into a Common Lisp workflow. Adding a
dispatch macro character (`#[section ...]`) fixes this at the cost of
adding undesirable extra syntactic noise.