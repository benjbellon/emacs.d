(require 'yasnippet)

(setq yas-snippet-dirs '("~/.emacs.d/snippets"))

(yas-global-mode 1)
;; Jump to end of snippet definition
(define-key yas-keymap (kbd "<return>") 'yas-next-field)
(define-key yas-keymap (kbd "C-x RET TAB") 'yas-exit-all-snippets)

(add-hook 'term-mode-hook (lambda()
			    (setq yas-dont-activate t)))

(provide 'setup-yasnippet)
