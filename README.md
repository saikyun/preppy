# This is a work in progress, do not use!
prepl-mode for emacs

### Currently working
```emacs-lisp
(preppy-connect "localhost:5555")
(preppy-write-message preppy-connection "(* 10 100)")`
(preppy-disconnect)
;; interactively
(preppy-eval-expression-at-point)
```

### ToDo
- Session management
- `expr-at-point` should include `#` (and similar) before sexp
