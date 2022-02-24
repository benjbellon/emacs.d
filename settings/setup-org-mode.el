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
                               "* TODO %i%?")
                              ("w" "Watchdog" entry
                               (file+headline org-watchdog-file "Watchdog")
                               "* %i%? \n %U")))

(setq org-agenda-custom-commands
      '(("b" "Bug" tags-todo "@bug"
         ((org-agenda-overriding-header "Bug")))))


(global-set-key (kbd "C-c c") 'org-capture)

;; org publish
(require 'ox-publish)

(defun publishing-entry (project)
  `(,project
    :base-directory ,(concat org-base-directory "/" project)
    :base-extension "org"
    :headline-levels 4
    :html-head-include-default-style nil
    :html-head-include-scripts nil
    :html-head-extra ,(concat "<link rel=\"stylesheet\" type=\"text/css\" href=\"/css/style.css\"")
    :html-mathjax nil
    :html-preamble nil
    :html-validation-link nil
    :publishing-directory ,(concat org-base-directory "/" project "/public/")
    :publishing-function org-html-publish-to-html
    :recursive t
    :section-numbers nil
    :time-stamp-file nil
    :with-toc nil
    :with-author nil
    :with-creator nil))

(defun publishing-static (project)
  `(,(concat project "-static")
    :base-directory ,(concat org-base-directory "/" project)
    :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
    :publishing-directory ,(concat org-base-directory "/" project "/public/")
    :recursive t
    :publishing-function org-publish-attachment))

(setq
 projects '("rse8.com")
 org-html-htmlize-output-type 'css)

(setq org-publish-project-alist
      `(
        ,@(mapcar (lambda (proj) (publishing-entry proj)) projects)
        ,@(mapcar (lambda (proj) (publishing-static proj)) projects)
        ("rse8-site" :components ("rse8.com" "rse8.com-static"))
        ))

(provide 'setup-org-mode)
