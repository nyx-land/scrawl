- [Scrawl](#orgb1491ad)
  - [A Note on Formatting](#org39cca79)
    - [A Caveat](#org1dd9e3c)


<a id="orgb1491ad"></a>

# Scrawl

Scrawl is a document markup format for Common Lisp that uses s-expressions, is implemented within CL using reader macros with the parser itself implemented using [parcom](https://github.com/fosskers/parcom/), and that uses [CommonDoc](https://commondoc.github.io/) as the document representation backend.

Please note that this project is still rather messy and incomplete, but the actual parser does work and is useful right now, which is why I'm releasing this publicly. There are still Common Doc nodes that I need to implement, and I will need to for Common Doc itself since the original author has abandoned it, but this is an active project that I plan to replace org-mode with and use extensively for my writing. There will be many forthcoming changes and improvements!


<a id="org39cca79"></a>

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


<a id="org1dd9e3c"></a>

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
