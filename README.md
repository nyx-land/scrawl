- [Scrawl](#org1a8e5fd)
  - [A Note on Formatting](#orgaebc93f)
    - [A Caveat](#org2132b93)


<a id="org1a8e5fd"></a>

# Scrawl

Scrawl is a document markup format for Common Lisp that uses s-expressions, is implemented within CL using reader macros, and that uses Common Doc as the document representation backend. It uses [parcom](https://github.com/fosskers/parcom/) to implement the actual parsing, and because it's implemented as reader macros in Common Lisp, you can use any CL implementation as the "compiler" backend for Scrawl.

Please note that this project is still rather messy and incomplete, but the actual parser does work and is useful right now, which is why I'm releasing this publicly. This is an active project that I plan to replace org-mode with and use extensively for my writing; there will be many forthcoming changes and improvements!


<a id="orgaebc93f"></a>

## A Note on Formatting

To enable formatting in Emacs, add the following to your \`.init.el\`:

```emacs-lisp
emacs-lisp

;; if using SLIME
(setq lisp-indent-function 'common-lisp-indent-function)

;; if using Sly
(setq lisp-indent-function 'sly-common-lisp-indent-function)

(modify-syntax-entry ?\[ "(]" lisp-mode-syntax-table)
(modify-syntax-entry ?\] ")[" lisp-mode-syntax-table)
```

Now everything will be indented nicely and is read as s-expressions the same as any other lisp code, which allows for Scrawl to be seamlessly integrated into a structural editing workflow.


<a id="org2132b93"></a>

### A Caveat

Scrawl will be considered in a 1.0 release state when it is possible to write something like `[# section \n\n etc]` and have it be formatted in Emacs the following way:

```
[# section title

 text with [* bold text]

 [# subsection

  more text]

 more text in higher level]
```

Every line should be indented to the start of the s-exp, but because of how lisp formatters work in Emacs and because of my limited knowledge of Emacs Lisp, there is no clear way that I am aware of as of now for how to read the brackets as s-exps but also have them formatted as plain data lists. I am not going to write an entire mode for something that is supposed to be able to be embedded within Common Lisp, so for the time being am recommending to add a dispatch character `#[# section ...]` to trick the formatter into indenting Scrawl nicely.
