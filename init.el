;; init.el --- initialize emacs configuration

;;; Author: Benj Bellon <benjaminbellon@gmail.com>
;;; Maintainer: Benj Bellon <benjaminbellon@gmail.com>
;;; Homepage: https://github.com/benjbellon/emacs.d
;;; Keywords: emacs, emacs.d, config

;;; Commentary:

;;; A few things of interest:
;;; 1. C-x C-c is overriden in the GUI so it redirects to the default
;;;    *shell* buffer. If you wish for this override while using
;;;    Emacs in terminal mode, provide the following ENV variable:
;;;      EMACS_OVERRIDE_C_X_C_C
;;;
;;; 2. init.el calls keychain-refresh-environment. If for some reason,
;;;    you do not wish for the following to be available to Emacs, add
;;;    a comment or remove the call:
;;;        SSH_AUTH_SOCK
;;;        SSH_AGENT_PID
;;;        GPG_AGENT_INFO
;;;
;;; Code:
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(require 'package)
(package-initialize)

(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(when (not (package-installed-p 'use-package)) (package-install 'use-package))

(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-dark t))

(use-package emacs
  :after solarized-theme
  :init
  (setq is-mac (equal system-type 'darwin))
  (setq enable-recursive-minibuffers t)

  (set-face-attribute 'default nil :font "Source Code Pro Medium" :height 90)

  (setq create-lockfiles nil)
  (setq backup-directory-alist
	`(("." .  ,(expand-file-name (concat user-emacs-directory "backups")))))
  (setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

  ;; Custom load paths, files, and directories
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (setq templates-dir (expand-file-name "templates" user-emacs-directory))
  (setq org-dir (expand-file-name "~/.org"))
  (setq org-dir-journal (expand-file-name "journal" org-dir))

  ;; get rid of the useles default connection string
  (setq sql-postgres-login-params nil)

  (add-to-list 'load-path templates-dir)

  (if (file-exists-p custom-file)
      (load custom-file)
    (message "custom.el does not yet exist, cannot load"))

  ;; Can't fix everything for everyone...
  (setq byte-compile-warnings '(not obsolete))
  (setq warning-suppress-log-types '((comp) (bytecomp)))
  (setq native-comp-async-report-warnings-errors 'silent)

  (setq x-select-enable-clipboard t)
  (setq dired-listing-switches "-lisah")

  (setq hl-line-mode 0)
  (setq indent-tabs-mode nil)

  (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

  (defalias 'yes-or-no-p 'y-or-n-p)

  :bind (("C-x C-b" . 'ibuffer)
	 ("C--" . 'undo)
	 ("C-x C-d" . 'find-file)
         ("C-x r m" . 'bookmark-set)
         ("C-x r d" . 'bookmark-delete)
	 ("C-l" . 'comint-clear-buffer))

  :config
  ;; Override C-x C-c to open the default shell buffer
  (if (display-graphic-p)
      (let ((shell-buffer "*shell*")
	    (quit-command "C-x C-c"))
	(if (not (get-buffer shell-buffer))
	    (shell))
	(define-key global-map (kbd quit-command)
		    (lambda () (interactive)
		      (delete-other-windows)
		      (switch-to-buffer "*shell*"))))))

(use-package ace-window
  :ensure t
  :init
  (setq aw-keys '(?a ?s ?d ?e ?f ?g ?h ?j ?k ?l))
  :bind ("C-x o" . 'ace-window))

(use-package apheleia
  :ensure t
  :config
  (setf (alist-get 'prettier apheleia-formatters) '("apheleia-npx" "prettier" "--stdin-filepath" filepath)))

(use-package avy
  :ensure t
  :init
  :bind (("M-g a a" . 'avy-goto-char-timer)
         ("M-g a l" . 'avy-goto-line)))

(use-package cargo-mode
  :ensure t)

(use-package cmake-mode
  :ensure t
  :mode "\\CMakeLists\\.txt\\'")

(use-package company
  :ensure t
  :init
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  :config
  (add-to-list 'company-backends 'company-capf)
  (global-company-mode))

(use-package consult
  :ensure t
  :bind ((:prefix-map consult-commands
		      :prefix-docstring "Consult mode quick commands"
		      :prefix "C-c c"
                      ("c b" . 'consult-bookmark)
	              ("c f" . 'consult-ripgrep)
	              ("c l" . 'consult-focus-lines)
                      ("c o" . 'consult-outline)
                      ("c r l" . 'consult-register-load)
                      ("c r s" . 'consult-register-store)
                      ("c r w" . 'consult-register)))
  :bind (("C-x b" . 'consult-buffer)
         ("M-g M-g" . 'consult-goto-line))
  :hook (completion-list-mode . consult-preview-at-point-mode))

(use-package dap-mode
  :bind (("C-c d" . dap-commands))
  :config
  (transient-define-prefix dap-commands ()
    "DAP mode commands"
    [["Commands"
      ("b" "Toggle Breakpoint" dap-breakpoint-toggle)
      ("c" "Continue" dap-continue)
      ("d" "Start debugger" dap-debug)
      ("i" "Step In" dap-step-in)
      ("n" "Next" dap-next)
      ("o" "Step Out" dap-step-out)
      ("s" "Stop" dap-stop-thread)
      ("u" "DAP UI commands" dap-ui-commands)]])

  (transient-define-prefix dap-ui-commands ()
    "DAP UI commands"
    [["Commands"
      ("B" "Breakpoints window" dap-ui-breakpoints)
      ("b" "Breakpoints" dap-ui-breakpoints-list)
      ("l" "Locals window" dap-ui-locals)
      ("r" "Repl window" dap-ui-repl)
      ("s" "Sessions window" dap-ui-sessions)
      ("W" "Show all windows" dap-ui-show-many-windows)
      ("w" "Hide all windows" dap-ui-hide-many-windows)]])

  :ensure t
  :config
  (require 'dap-lldb)
  (setq dap-lldb-debug-program '("/bin/lldb-dap")))

(use-package emacs-lisp-mode
  :mode "\\.dir-locals\\.el\\'")

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)))

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package git-timemachine
  :ensure t)

(use-package go-mode
  :after lsp-mode
  :ensure t
  :hook ((go-mode . lsp-mode)
	 (python-mode . lsp)
	 (before-save . lsp-format-buffer)
	 (before-save . lsp-organize-imports)))

(use-package just-mode
  :ensure t)

(use-package keychain-environment
  :ensure t
  :config
  (keychain-refresh-environment))

(use-package fountain-mode
  :ensure t
  :mode "\\.fountain?\\'"
  :config
  :hook (fountain-mode . olivetti-mode))

(use-package flycheck
  :ensure t)

(use-package gptel
  :ensure t
  :bind (:prefix-map ai-commands
		     :prefix-docstring "Emacs AI comamands"
		     :prefix "C-c a"
		     ("a" . 'gptel)
		     ("s" . 'gptel-menu)
		     ("RET" . 'gptel-send))
  :init
  (gptel-make-openai "openai"
    :stream t
    :key (auth-source-pass-get 'secret "openai.com/api.openai.com/apikey"))

  (defvar gptel--anthropic
    (gptel-make-anthropic "anthropic"
      :stream t
      :key (auth-source-pass-get 'secret "anthropic.com/api.anthropic.com/apikey")))

  (gptel-make-openai "groq"
    :host "api.groq.com"
    :endpoint "/openai/v1/chat/completions"
    :stream t
    :key (auth-source-pass-get 'secret "groq.com/api.groq.com/apikey")
    :models '("mixtral-8x7b-32768"
	      "gemma-7b-it"
	      "llama2-70b-4096"))

  (gptel-make-gemini "gemini"
    :stream t
    :key (auth-source-pass-get 'secret "google.com/aiplatform.googleapis.com/gemini/apikey"))

  (setq gptel-backend gptel--anthropic
        gptel-model 'claude-3-7-sonnet-20250219
	gptel-default-mode #'org-mode))

(use-package hl-todo
  :ensure t
  :init
  (global-hl-todo-mode))

(use-package jinx
  :ensure t
  :hook ((text-mode . jinx-mode)
	 (org-mode . jinx-mode))
  :bind (("<f2>" . 'jinx-correct)
	 ("M-$" . 'jinx-correct)
	 ("C-M-$" . 'jinx-languages)))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :bind-keymap ("C-c l" . lsp-command-map)
  :hook (c-mode . lsp))

(use-package magit
  :ensure t
  :bind ("C-x g" . 'magit-status))

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
	      ("M-A" . 'marginalia-cycle))
  :init
  (marginalia-mode))

(use-package message
  :after smtpmail-multi
  :init
  (defun get-mail-accounts ()
    (let* ((entries (auth-source-pass-parse-entry "emacs/mail-accounts"))
	   (accounts (if (null entries) nil (seq-filter (lambda (pair) (not (equal (car pair) 'secret))) entries))))
      (if (null accounts)
          (message "No mail accounts were found in the password-store. Cannot send mail")
	(mapcar (lambda (pair)
		  (let ((key (intern (car pair)))
			(value ((lambda (account)
				  (let ((store (auth-source-pass-parse-entry account)))
				    `(,(assoc-default "user" store)
				      ,(assoc-default "host" store)
				      ,(string-to-number (assoc-default "port" store))
				      ,(assoc-default "user" store)
				      ,(intern (assoc-default "type" store))
				      nil nil nil
				      )))
				(cdr pair))))
		    (cons key value)))
		accounts))))

  (setq send-mail-function 'smtpmail-multi-send-it
	mail-accounts (get-mail-accounts)
	primary-account (nth 0 (car mail-accounts))
	smtpmail-multi-accounts mail-accounts
	smtpmail-multi-default-account primary-account
	user-mail-address (nth 3 (cdr (assoc primary-account smtpmail-multi-accounts)))
	smtpmail-multi-associations (mapcar (lambda (pair) `(,(car (cdr pair)) ,(car pair))) mail-accounts)))

(use-package multiple-cursors
  :ensure t
  :init
  (setq mc/always-run-for-all t)
  :bind (:prefix-map mc-commands
		     :prefix-docstring "Multiple cursors quick commands"
		     :prefix "C-c m"
		     ("i" . 'mc/insert-numbers)
		     ("l" . 'mc/edit-lines)
		     ("m" . 'mc/mark-all-like-this)
		     ("n" . 'mc/mark-next-like-this)
		     ("p" . 'mc/mark-previous-like-this)
		     ("r r" . 'mc/mark-all-in-region-regexp)))

(use-package nerd-icons
  :ensure t
  :after marginalia
  :hook (marginalia . nerd-icons-completion-marginalia-setup)
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package nerd-icons-completion
  :ensure t
  :config
  (nerd-icons-completion-mode))

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nginx-mode
  :ensure t)

(use-package ob-restclient
  :ensure t)

(use-package olivetti
  :ensure t
  :custom
  (olivetti-body-width 250)
  :mode (("\\.txt?\\'" . olivetti-mode)
	 ("\\README.md\\'" . olivetti-mode)
	 ("\\.org?\\'" . olivetti-mode)))

(use-package orderless
  :ensure t
  :init
  (setq orderless-matching-styles '(orderless-flex))
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides'((file (styles basic partial-completion)))))

(use-package org-ai
  :after password-store
  :ensure t
  :bind (("C-c M-a h" . 'org-ai-prompt))
  :init
  (org-ai-global-mode)
  (setq org-ai-openai-api-token (password-store-get "openai.com/api.openai.com/apikey")
	org-ai-default-chat-model "gpt-4-turbo-preview"))

(use-package org-journal
  :ensure t
  :init
  (setq org-journal-dir org-dir-journal)
  (setq org-journal-file-type 'weekly)
  (setq org-journal-file-format "%Y%m%d.org")
  (setq org-journal-enable-agenda-integration t))

(use-package org-mode
  :after (:all org-ai org-journal ob-restclient)
  :mode "\\.org$"
  :bind (:prefix-map org-commands
		     :prefix-docstring "Org mode quick commands"
		     :prefix "C-c o"
		     ("j o" . 'org-journal-open-current-journal-file)
		     ("j n" . 'org-journal-new-entry)
		     ("j t" . 'org-journal-new-scheduled-entry))

  :hook ((org-mode . hl-todo-mode)
	 (org-mode . olivetti-mode)
         (org-mode . org-ai-mode)
         (org-mode . org-indent-mode))
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t )
     (python . t)
     (restclient .t )
     (shell . t)
     (sql . t)))

  (setq org-ai-image-directory (expand-file-name "org-ai-images/" org-dir))

  :custom-face
  (org-level-1 ((t (:family "Roboto" :height 120))))
  (org-level-2 ((t (:family "Roboto" :height 110))))
  (org-level-3 ((t (:family "Roboto" :height 100))))
  (org-document-title ((t (:family "Roboto Slab" :height 220 :weight medium))))
  (org-document-info ((t (:family "Roboto Slab" :height 110 :weight medium))))

  :custom
  (org-auto-align-tags t)
  (org-hidden-keywords '(title author date startup))
  (org-startup-folded nil)
  (org-hide-leading-stars t)
  (org-hide-emphasis-markers t)
  (org-num-face nil)
  (org-level-color-stars-only nil)

  (org-todo-keywords
   `((sequence "BACKLOG(b)" "BLOCKED(l)" "TODO(t)" "IN_PROGRESS(i)" "|" "DONE(d)" "CANCELLED(c)")))

  (org-confirm-babel-evaluate nil))

(use-package org-superstar
  :ensure t
  :after org
  :hook (org-mode . org-superstar-mode)
  :init
  (setq org-superstar-headline-bullets-list '("◉" "○" "◉" "○" "◉"))
  (setq org-superstar-special-todo-items t)
  (setq org-superstar-prettify-item-bullets t)
  (setq org-superstar-item-bullet-alist '((?* . ?*) (?+ . ?◦) (?- . ?•))))

(use-package ox-hugo
  :ensure t
  :after ox)

(use-package pass
  :ensure t)

(use-package paredit
  :ensure t
  :hook
  (lisp-mode . paredit-mode)
  (emacs-lisp-mode . paredit-mode))

(use-package password-store
  :ensure t
  :config
  ;; don't append, since we /only/ want password-store based credentials
  (setq auth-sources '(password-store)))

(use-package pest-mode
  :ensure t)

(use-package projectile
  :ensure t
  :init (projectile-mode +1)
  :bind (("C-c p" . 'projectile-command-map)
	 ("C-c p s g" . 'projectile-ripgrep)
	 ("C-c p s r" . 'projectile-replace-regexp)))

(use-package projectile-ripgrep
  :ensure t)

(use-package restclient
  :ensure t)

(use-package rustic
  :ensure t)

(use-package smtpmail-multi
  :ensure t)

(use-package treemacs
  :ensure t
  :after transient
  :bind (("C-c T" . treemacs-dispatch))
  :config
  (transient-define-prefix treemacs-dispatch ()
    "Treemacs command list"
    [["Commands"
      ("t" "Toggle treemacs" treemacs)
      ("s" "Select treemacs window" treemacs-select-window)
      ("f" "Find file in treemacs" treemacs-find-file)]]))

(use-package treemacs-nerd-icons
  :ensure t
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package undo-tree
  :ensure t)

(use-package vertico
  :ensure t
  :init
  (defun my/go-home ()
    (interactive)
    (cond
     ((looking-back "/") (insert "~/"))
     (:else (insert "~"))))
  (vertico-mode)
  :custom
  (vertico-count 25)
  (vertico-resize t)
  :bind (:map vertico-map ("~" . 'my/go-home)))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
	      ("C-d" . 'vertico-exit)
	      ("DEL" . 'vertico-directory-delete-char))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package uuidgen
  :ensure t)

(use-package visual-regexp
  :ensure t
  :bind (("C-s" . 'vr/isearch-forward)
	 ("C-r" . 'vr/isearch-backward)
	 ("C-M-r" . 'vr/replace)))

(use-package visual-regexp-steroids
  :after visual-regexp
  :ensure t)

(use-package web-mode
  :ensure t
  :mode (("\\.svelte?\\'" . web-mode)
	 ("\\.html?\\'" . web-mode)
	 ("\\.js?\\'" . web-mode)
	 ("\\.ts?\\'" . web-mode))
  :config
  (setq web-mode-engines-alist '(("svelte" . ".svelte$")))

  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-script-padding 2)
  (web-mode-style-padding 2)
  (web-mode-block-padding 2)

  :hook ((web-mode . lsp)
         (web-mode . apheleia-mode)))

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

(use-package ws-butler
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . ws-butler-mode))

(use-package yasnippet
  :ensure t
  ;; :bind (("<C-tab>" . 'yas-expand))
  :config
  (yas-global-mode t))
