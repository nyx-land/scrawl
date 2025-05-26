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


## A Note on Formatting

Scrawl will be considered in a 1.0 release state when it is possible
to write something like `[# section \n\n etc]` and have it be
formatted in Emacs the following way:

```
[# section title

 text with [* bold text]

 [# subsection

  more text]

 more text in higher level]
```

Every line should be indented to the start of the s-exp, but because
of how lisp formatters work in Emacs and because of my limited
knowledge of Emacs Lisp, there is no clear way that I am aware of as
of now for how to read the brackets as s-exps but also have them
formatted as plain data lists. I am not going to write an entire mode
for something that is supposed to be able to be embedded within Common
Lisp, so for the time being am recommending to add a dispatch
character `#[# section ...]` to trick the formatter into indenting
Scrawl nicely.