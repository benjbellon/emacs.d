(require 'typescript-mode)

(defun indent-offset-ts ()
     (setq typescript-indent-level 2))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
(add-hook 'typescript-mode-hook 'indent-offset-ts)


(provide 'setup-typescript-mode)
;;; setup-typescript-mode.el ends here
