;;; setup-company-mode.el --- company mode config    -*- lexical-binding: t; -*-

;;; Commentary:

;; basic company mode configs

;;; Code:
(require 'company)

;; When working with a larger project, until a parser is written
;; for compile_commands.json DB, you can add the headers to
;; .dir-locals.el as follows:
;; (company-clang-arguments . ("-I/path/to/include"))

(setq company-idle-delay 0.1)
(setq company-clang-executable "/usr/bin/clang++")

(add-hook 'after-init-hook 'global-company-mode)

(provide 'setup-company-mode)
;;; setup-company-mode.el ends here
