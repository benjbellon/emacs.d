(require 'bindKeys)
(require 'treemacs)

(bindKeys '("t")
          '(("t" . treemacs-select-window)
            ("b" . treemacs-bookmark)
            ("f" . treemacs-find-file)
            ("d" . treemacs-delete-other-windows)))


(provide 'setup-treemacs)
;;; setup-treemacs.el ends here
