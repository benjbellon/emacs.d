(require 'find-file-in-project)

(define-key global-map (kbd "M-SPC") 'find-file-in-project-by-selected)
(define-key global-map (kbd "C-x M-SPC") 'find-file-in-project-at-point)

(provide 'setup-find-file-in-project)
;;; setup-find-file-in-project ends here
