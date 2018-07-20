;;; setup-txt-mode.el --- basic txt mode config      -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic text mode config

;;; Code:

(defun setup-margins ()
  "Setup margins for text mode."
  (setq left-margin-width 20)
  (setq right-margin-width 20))

(defun text-configs ()
  "Setup basic text configs"
  (visual-line-mode t)
  (flyspell-mode t)
  (setup-margins))


(add-hook 'text-mode-hook 'text-configs)

(provide 'setup-txt-mode)
;;; setup-txt-mode.el ends here
