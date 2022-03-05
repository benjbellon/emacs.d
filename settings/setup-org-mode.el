;; Pretty bullets
(global-set-key (kbd "C-c a") 'org-agenda)

(require 'rainbow-mode)
(add-hook 'org-mode-hook 'rainbow-mode)

(setq org-directory "~/workspace/.org")
(setq org-base-directory "~/workspace/.org")

(setq
 org-export-html-postamble nil
 org-log-done 'note
 org-todo-keywords '((sequence "Todo(t)" "In Progress(w)" "Blocked(b)" "|" "DONE(d)" "CANCELLED(c)"))

 org-inbox-file (concat org-base-directory "/inbox.org")
 org-projects-file (concat org-base-directory "/projects.org")
 org-someday-file (concat org-base-directory "/someday.org")
 org-watchdog-file (concat org-base-directory "/watchdog.org")

 org-agenda-files `(,org-inbox-file ,org-projects-file ,org-watchdog-file)
 org-refile-targets '((org-projects-file :maxlevel . 3)
                      (org-someday-file :level . 1)
                      (org-watchdog-file :maxlevel . 2)))


;; setup: org-capture
(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline org-inbox-file "Tasks")
                               "*** Todo %i %?")))

(setq org-agenda-custom-commands
      '(("b" "Bug" tags-todo "@bug"
         ((org-agenda-overriding-header "Bug")))))


(global-set-key (kbd "C-c C-o c") 'org-capture)
(global-set-key (kbd "C-c C-o o") '(lambda () (interactive) (find-file org-base-directory)))
(global-set-key (kbd "C-c C-o i") '(lambda () (interactive) (find-file (concat org-base-directory "/inbox.org"))))

(provide 'setup-org-mode)
