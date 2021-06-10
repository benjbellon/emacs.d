(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm
  (setq-local buffer-save-without-query t))

(setq rustic-format-on-save t)
(add-hook 'rustic-mode-hook 'rk/rustic-mode-hook)

(provide 'setup-rustic-mode)
;;; setup-rust-mode.el ends here
