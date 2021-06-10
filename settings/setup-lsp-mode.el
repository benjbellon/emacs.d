;;; setup-lsp-mode.el --- lsp setup                  -*- lexical-binding: t; -*-
(setq lsp-keymap-prefix "C-c l")
(define-key lsp-mode-map (kbd "C-c l") lsp-command-map)

(require 'lsp-mode)
(require 'cquery)

(setq cquery-executable "/usr/local/bin/cquery")

(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq lsp-completion-provider :capf)

(lsp-treemacs-sync-mode 1)

(add-hook 'c-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)
(add-hook 'dart-mode-hook #'lsp)
(add-hook 'lisp-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)
(add-hook 'rust-mode #'lsp)

(add-hook 'lsp-after-open-hook 'lsp-ui-mode)

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (define-key lsp-mode-map (kbd "C-c l t d") 'lsp-dart-show-outline)
  (define-key lsp-mode-map (kbd "C-c l t f") 'lsp-dart-show-flutter-outline))

(provide 'setup-lsp-mode)
;;; setup-lsp-mode.el ends here
