;;; setup-company-mode.el --- company mode config    -*- lexical-binding: t; -*-

;;; Commentary:

;; basic company mode configs

;;; Code:

(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.1)


(provide 'setup-company-mode)
;;; setup-company-mode.el ends here
