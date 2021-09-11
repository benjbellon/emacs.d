;;; setup-dap-mode.el ---                            -*- lexical-binding: t; -*-
(with-eval-after-load 'dap-mode
  (define-key dap-mode-map (kbd "C-c d d") 'dap-hydra))

(provide 'setup-dap-mode)
;;; setup-dap-mode.el ends here
