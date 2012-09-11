;;; ruby-dev-mode.el — Minor mode to add keybindings for using ruby-dev

(require 'ruby-dev-eval)
(require 'ruby-dev-doc)
(require 'ruby-dev-repl)

;;;###autoload
(defvar ruby-dev-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-e") 'ruby-dev-eval-last-sexp)
    (define-key map (kbd "C-c C-e") 'ruby-dev-eval-last-sexp)
    (define-key map (kbd "C-c C-b") 'ruby-dev-eval-buffer)
    (define-key map (kbd "C-c C-r") 'ruby-dev-eval-region)
    (define-key map (kbd "C-c C-s") 'ruby-dev-eval-string)
    (define-key map (kbd "C-c C-d") 'ruby-dev-show-doc)
    (define-key map (kbd "C-c TAB") (lambda ()
                                      (interactive)
                                      (ruby-dev-start-repl "main" "TOPLEVEL_BINDING")))
    (define-key map (kbd "C-c S-TAB") 'ruby-dev-start-repl)
    map)
  "Keybindings for `ruby-dev-mode'.")

;;;###autoload
(define-minor-mode ruby-dev-mode
  "Minor mode for live features in ruby-mode.

\\{ruby-dev-mode-map}"
  :lighter " Dev")

;;;###autoload
(defun turn-on-ruby-dev ()
  (interactive)
  (ruby-dev-mode 1))

;;;###autoload
(defun turn-off-ruby-dev ()
  (interactive)
  (ruby-dev-mode -1))

(provide 'ruby-dev-mode)
