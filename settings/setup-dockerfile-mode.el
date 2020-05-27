;;; setup-dockerfile-mode.el ---                     -*- lexical-binding: t; -*-

(add-hook 'dockerfile-mode-hook
          (lambda ()
            (setq tab-width 2)))

(provide 'setup-dockerfile-mode)
;;; setup-dockerfile-mode.el ends here
