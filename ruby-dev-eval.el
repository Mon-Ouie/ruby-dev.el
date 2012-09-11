;;; ruby-dev-eval.el — Functions to evaluate Ruby code.

(require 'ruby-dev-core)
(require 'ruby-dev-error)

;;;###autoload
(defun ruby-dev-eval-string (code &optional filename line)
  "Evaluates an arbitrary string of ruby code.

Optionally, you can specify a FILENAME (__eval__ by default) and a LINE number
 (0 by default)."
  (interactive "sEval Ruby: ")
  (ruby-dev-ensure)
  (ruby-dev-send-request "eval" :code code
                         :filename (or filename "__eval__")
                         :line (or line 0))
  (let ((response (ruby-dev-read-response)))
    (with-ruby-dev-data (success result) response
      (if (eql success :json-false) (ruby-dev-show-error response)
        (message "%s" result)))))

(defun ruby-dev-find-filename ()
  "Attempts to find the filename to use for code evaluated from the current buffer.

If the `buffer-file-name' is set, it is used; otherwise, it defaluts to __eval__."
  (or (buffer-file-name) "__eval__"))

;;;###autoload
(defun ruby-dev-eval-region (start end &optional filename line)
  "Tries to evaluate a region of code.

FILENAME and LINE are normally guessed from the buffer and the location of START,
but they can be specified explicitly."
  (interactive "r")
  (ruby-dev-ensure)
  (unless filename (setq filename (ruby-dev-find-filename)))
  (unless line (setq line (line-number-at-pos start)))
  (ruby-dev-eval-string (buffer-substring start end) filename line))

;;;###autoload
(defun ruby-dev-eval-last-sexp (&optional filename line)
  "Evaluates the last 'sexp' in code.

Sexps are found using movement functions from `ruby-mode'."
  (interactive)
  (ruby-dev-ensure)
  (let (start end)
    (save-excursion
      (ruby-backward-sexp)
      (setq start (point))
      (ruby-forward-sexp)
      (setq end (point)))
    (ruby-dev-eval-region start end filename line)))

;;;###autoload
(defun ruby-dev-eval-buffer (&optional filename)
  "Evaluates the whole buffer.

An explicit FILENAME can be specified, otherwise __eval__ is used."
  (interactive)
  (ruby-dev-ensure)
  (unless filename (setq filename (ruby-dev-find-filename)))
  (ruby-dev-eval-string (buffer-string) filename 1))

(provide 'ruby-dev-eval)
