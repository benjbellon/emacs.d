;;; setup-lsp-mode.el --- lsp setup                  -*- lexical-binding: t; -*-
(setq lsp-keymap-prefix "C-c l")
(define-key lsp-mode-map (kbd "C-c l") lsp-command-map)

(require 'lsp-mode)
(require 'cquery)

(setq cquery-executable "/usr/local/bin/cquery")

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq lsp-completion-provider :capf)

(add-hook 'c-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)
(add-hook 'dart-mode-hook #'lsp)
(add-hook 'lisp-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)

(add-hook 'lsp-after-open-hook 'lsp-ui-mode)

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

(provide 'setup-lsp-mode)
;;; setup-lsp-mode.el ends here
