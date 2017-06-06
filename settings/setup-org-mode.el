;; Pretty bullets
(add-hook 'org-mode-hook (lambda() (org-bullets-mode 1)))

(setq org-ellipsis "⤳")
(setq org-log-done 'time)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

(global-set-key (kbd "C-c a") 'org-agenda)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

(provide 'setup-org-mode)
