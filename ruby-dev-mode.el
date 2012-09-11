;;; ruby-dev-mode.el — Minor mode to add keybindings for using ruby-dev

(require 'ruby-dev-eval)
(require 'ruby-dev-doc)
(require 'ruby-dev-repl)

(defvar ruby-dev-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-e") 'ruby-dev-eval-last-sexp)
    (define-key map (kbd "C-c C-e") 'ruby-dev-eval-last-sexp)
    (define-key map (kbd "C-c C-b") 'ruby-dev-eval-buffer)
    (define-key map (kbd "C-c C-r") 'ruby-dev-eval-region)
    (define-key map (kbd "C-c C-s") 'ruby-dev-eval-string)
    (define-key map (kbd "C-c C-d") 'ruby-dev-show-doc)
    (define-key map (kbd "C-c C-i") 'ruby-dev-start-repl)
    map)
  "Keybindings for `ruby-dev-mode'.")

(define-minor-mode ruby-dev-mode
  "Minor mode for live features in ruby-mode."
  :lighter " Dev")

(defun turn-on-ruby-dev ()
  (interactive)
  (ruby-dev-mode 1))

(defun turn-off-ruby-dev ()
  (interactive)
  (ruby-dev-mode -1))

(provide 'ruby-dev-mode)
