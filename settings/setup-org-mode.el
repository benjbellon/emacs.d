;; Pretty bullets
(add-hook 'org-mode-hook (lambda() (org-bullets-mode 1)))

(setq org-log-done 'time)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "BLOCKED" "DONE")))
(setq org-tag-alist
      '((:startgroup . nil)
	("blocker" . ?b)
	("!critical" . ?u)
	("!major" . ?m)
	("!minor" . ?n)
	("!trivial" . ?t)
	(:endgroup . nil)
	("@home" . ?h)
	("@store" . ?s)
	("@work" . ?w)
	("reading" . ?r)
	("studying" . ?s)
	("implementation" . ?i)))

(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-agenda-files (list "~/Dropbox/org/todo_work.org"
                             "~/Dropbox/org/todo_home.org"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

(provide 'setup-org-mode)
