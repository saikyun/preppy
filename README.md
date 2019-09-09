# This is a work in progress, do not use!
prepl-mode for emacs

### Currently working
```emacs-lisp
(preppy-connect "localhost:5555")
(preppy-write-message preppy-connection "(* 10 100)")`
(preppy-disconnect)
;; interactively
(preppy-start-clojure-and-connect) ;; starts `clojure` with arguments to start a prepl-server at localhost:5555
(preppy-eval-expression-at-point) ;; sends the expression to that prepl-server
(preppy-stop-clojure) ;; stops the clojure process
```

### ToDo
- Session management
- `expr-at-point` should include `#` (and similar) before sexp
