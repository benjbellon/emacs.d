;;; setup-lsp-mode.el --- lsp setup                  -*- lexical-binding: t; -*-
(setq lsp-keymap-prefix "C-c l")

(require 'lsp-mode)
(require 'cquery)

(setq cquery-executable "/usr/local/bin/cquery")

(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq lsp-completion-provider :capf)

;; disable lsp-ui-doc-mode due to very slow update performance
;; TRACK: https://github.com/emacs-lsp/lsp-ui/issues/613
(setq lsp-ui-doc-enable nil)
(setq lsp-clients-clangd-args '("--compile-commands-dir=build"))
(setq lsp-disabled-clients '())


(lsp-treemacs-sync-mode 1)

(add-hook 'c-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)
(add-hook 'dart-mode-hook #'lsp)
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'lisp-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)
(add-hook 'rustic-mode #'lsp)
(add-hook 'web-mode-hook #'lsp)
(add-hook 'typescript-mode-hook #'lsp)

(add-hook 'lsp-after-open-hook 'lsp-ui-mode)

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (define-key lsp-mode-map (kbd "C-c l u f") 'lsp-ui-imenu)
  (define-key lsp-mode-map (kbd "C-c l t d") 'lsp-dart-show-outline)
  (define-key lsp-mode-map (kbd "C-c l t f") 'lsp-dart-show-flutter-outline))

(provide 'setup-lsp-mode)
;;; setup-lsp-mode.el ends here
