(require 'dash)
(require 'bindKeys)
(require 'treemacs)

;; (setq treemacs-dir
;;       (car (-filter (lambda (path)
;;                       (string-match-p "^.+.emacs.d/elpa/treemacs-[0-9]+.[0-9]+$" path))
;;                     load-path)))

(bindKeys '("t")
          '(("i" . treemacs)
            ("t" . treemacs-select-window)
            ("b" . treemacs-bookmark)
            ("f" . treemacs-find-file)
            ("d" . treemacs-delete-other-windows)))

(provide 'setup-treemacs)
;;; setup-treemacs.el ends here
