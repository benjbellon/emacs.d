;;; setup-lsp-mode.el --- lsp setup                  -*- lexical-binding: t; -*-
(require 'lsp-mode)
(require 'cquery)

(setq cquery-executable "/usr/local/bin/cquery")

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq lsp-completion-provider :capf)

(add-hook 'c-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)

(add-hook 'lsp-after-open-hook 'lsp-ui-mode)

(provide 'setup-lsp-mode)
;;; setup-lsp-mode.el ends here
