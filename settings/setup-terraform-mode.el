(add-hook 'terraform-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'terraform-format-buffer)))

(provide 'setup-terraform-mode)
;;; setup-terraform-mode.el ends here
