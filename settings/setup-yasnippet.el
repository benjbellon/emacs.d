(require 'yasnippet)

(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)

(define-key yas-minor-mode-map [(tab)] nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)

;; Jump to end of snippet definition
(define-key yas-keymap (kbd "<return>") 'yas-exit-all-snippets)

(add-hook 'term-mode-hook (lambda()
			    (setq yas-dont-activate t)))

(provide 'setup-yasnippet)
