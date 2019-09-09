;; most code taken from `monroe-mode`

(require 'edn)
(require 'subr-x)
(eval-when-compile
  (require 'cl))

(defvar preppy-project-file "deps.edn")
(defvar preppy-project-dir)
(defvar preppy-connection)

(defun preppy-set-project-dir ()
  "Sets `preppy-project-dir` to the project directory."
  (interactive)
  (setq preppy-project-dir
        (locate-dominating-file default-directory
                                preppy-project-file)))

(defun preppy-write-message (process message)
  "Send message to given process."
  (process-send-string process message))

(defvar preppy-last-result)

(defun preppy-net-filter (process str)
  "Called when a new message is recieved."
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert str)
    (let ((end (point)))
      (backward-sexp)
      (let ((start (point)))
        (let ((res (edn-read (buffer-substring start end))))
          (setq preppy-last-result res)
          (message (gethash ':val preppy-last-result)))))))

(defun preppy-str-or-default (str default)
  (if (and str (not (string= "" str)))
      str
    default))

(defun preppy-strip-protocol (host)
  "Check if protocol was given and strip it."
  (let ((host (replace-regexp-in-string "[ \t]" "" host)))
    host))

(defun preppy-connect (host-and-port)
  "Connect to a prepl server."
  (let* ((hp   (split-string (preppy-strip-protocol host-and-port) ":"))
         (host (preppy-str-or-default (first hp) "localhost"))
         (port (string-to-number (preppy-str-or-default (second hp) "5555"))))
    (message "Connecting to prepl host on '%s:%d'..." host port)
    (let ((process (open-network-stream "preppy" "*preppy-connection*" host port)))
      (set-process-filter process 'preppy-net-filter)
      (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
      (setq preppy-connection process)
      (message "Connected to prepl host on '%s:%d'!" host port)
      process)))

(defun preppy-start-clojure ()
  (if (get-buffer "*preppy-clojure*")
      (message "Clojure is already started.")
    (let ((default-directory (or preppy-project-dir default-directory)))
      (async-shell-command "clojure -J-Dclojure.server.jvm=\"{:port 5555 :accept clojure.core.server/io-prepl}\"" "*preppy-clojure*")
      (message "Started Clojure in dir %s with prepl on port %d" default-directory 5555))))

(defun preppy-stop-clojure ()
  (interactive)
  (if-let ((buf (get-buffer "*preppy-clojure*")))
      (kill-buffer buf)
    (message "Clojure isn't running.")))

(defun preppy-repeat-connect (host-and-port &optional times)
  (let ((times (or times 5)))
    (if (<= times 0)
        (message "Couldn't connect to %s after 5 seconds." host-and-port)
      (condition-case nil
          (preppy-connect host-and-port)
        (error (progn
                 (message "Couldn't connect to %s, trying again..." host-and-port)
                 (run-at-time "1 sec" nil 'preppy-repeat-connect "localhost:5555" (- times 1))))))))

(defun preppy-start-clojure-and-connect ()
  (interactive)
  (preppy-set-project-dir)
  (preppy-start-clojure)
  (run-at-time "3 sec" nil 'preppy-repeat-connect "localhost:5555"))

(defun preppy-disconnect ()
  "Disconnects from the current prepl connection."
  (let ((p (get-buffer-process "*preppy-connection*")))
    (when (and p (process-live-p p))
      (delete-process p)))
  (setq preppy-connection nil))

(defun preppy-input-sender (proc input)
  "Called when user enter data in REPL and when something is received in."
  (preppy-write-message proc (format "(do %s \n)" input)))

(defun preppy-eval-region (start end)
  "Evaluate selected region."
  (interactive "r")
  (preppy-input-sender
   preppy-connection
   (buffer-substring-no-properties start end)))

(defun preppy-eval-expression-at-point ()
  "Figure out expression at point and send it for evaluation."
  (interactive)
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (preppy-eval-region (point) end))))

;;(preppy-connect "localhost:5555")
;;(preppy-write-message preppy-connection "(* 10 100)")
;;(preppy-disconnect)

;;(preppy-stop-clojure)
;;(preppy-start-clojure-and-connect)
