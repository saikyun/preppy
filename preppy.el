;; most code taken from `monroe-mode`

(defun preppy-write-message (process message)
  "Send message to given process."
  (process-send-string process message))

(defun preppy-net-filter (process str)
  "Called when a new message is recieved."
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert str)))

(defun preppy-str-or-default (str default)
  (if (and str (not (string= "" str)))
      str
    default))

(defun preppy-strip-protocol (host)
  "Check if protocol was given and strip it."
  (let ((host (replace-regexp-in-string "[ \t]" "" host)))
    host))

(defvar preppy-connection)

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
      process)))

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

(preppy-connect "localhost:5555")
(preppy-write-message preppy-connection "(* 10 100)")
;;(preppy-disconnect)

