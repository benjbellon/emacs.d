;; init.el --- initialize emacs configuration

;;; Author: Benj Bellon <benjaminbellon@gmail.com>
;;; Maintainer: Benj Bellon <benjaminbellon@gmail.com>
;;; Homepage: https://github.com/benjbellon/emacs.d
;;; Keywords: emacs, emacs.d, config

;;; Commentary:

;;; A few things of interest:
;;; 1. C-x C-c is overriden in the GUI so it redirects to the default
;;;    *ansi-term* buffer. If you wish for this override while using
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

(use-package emacs
  :init
  (setq is-mac (equal system-type 'darwin))
  (setq enable-recursive-minibuffers t)

  (set-face-attribute 'default nil :font "Source Code Pro Medium" :height 90)

  (setq create-lockfiles nil)
  (setq backup-directory-alist
	`(("." . (expand-file-name
		  (concat user-emacs-directory "backups")))))
  (setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))


  ;; Custom load paths, files, and directories
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (setq templates-dir (expand-file-name "templates" user-emacs-directory))

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

  :bind (
	 ("C-x C-b" . 'ibuffer)
	 ("C--" . 'undo)
	 ("C-x C-d" . 'find-file)
         ("C-x r m" . 'bookmark-set)
         ("C-x r d" . 'bookmark-delete))

  :config
  ;; Override C-x C-c to open the default ansi-term buffer
  (if (display-graphic-p)
      (let ((ansi-buffer "*ansi-term*")
	    (quit-command "C-x C-c"))
	(if (not (get-buffer ansi-buffer))
	    (ansi-term "/bin/bash"))
	(define-key global-map (kbd quit-command)
		    (lambda () (interactive)
		      (delete-other-windows)
		      (switch-to-buffer "*ansi-term*")))))
  :hook (before-save . delete-trailing-whitespace))

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
  :bind (("C-c c b" . 'consult-bookmark)
	 ("C-c c f" . 'consult-ripgrep)
	 ("C-c c l" . 'consult-focus-lines)
         ("C-c c o" . 'consult-outline)
         ("C-c c r l" . 'consult-register-load)
         ("C-c c r s" . 'consult-register-store)
         ("C-c c r w" . 'consult-register)
	 ("C-x b" . 'consult-buffer)
         ("M-g g" . 'consult-goto-line))
  :hook (completion-list-mode . consult-preview-at-point-mode))

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)))

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

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

(use-package hl-todo
  :ensure t
  :init
    (global-hl-todo-mode))

(use-package lsp-mode
  :ensure t
  :commands lsp)

(use-package magit
  :ensure t
  :bind ("C-x g" . 'magit-status))

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
	      ("M-A" . 'marginalia-cycle))
  :init
  (marginalia-mode))

(use-package multiple-cursors
  :ensure t
  :init
  (setq mc/always-run-for-all t)
  :bind (("C-c m i" . 'mc/insert-numbers)
	 ("C-c m l" . 'mc/edit-lines)
	 ("C-c m m" . 'mc/mark-all-like-this)
	 ("C-c m n" . 'mc/mark-next-like-this)
	 ("C-c m p" . 'mc/mark-previous-like-this)
	 ("C-c m r r" . 'mc/mark-all-in-region-regexp)))

(use-package nginx-mode
  :ensure t)

(use-package git-timemachine
  :ensure t)

(use-package paredit
  :ensure t
  :hook
  (lisp-mode . paredit-mode)
  (emacs-lisp-mode . paredit-mode))

(use-package projectile
  :ensure t
  :init (projectile-mode +1)
  :bind (("C-c p" . 'projectile-command-map)
	 ("C-c p s g" . 'projectile-ripgrep)
	 ("C-c p s r" . 'projectile-replace-regexp)))

(use-package projectile-ripgrep
  :ensure t)

(use-package olivetti
  :ensure t
  :custom
  (olivetti-body-width 120)
  :mode (("\\.txt?\\'" . olivetti-mode)
	 ("\\README.md\\'" . olivetti-mode)))

(use-package orderless
  :ensure t
  :init
  (setq orderless-matching-styles '(orderless-flex))
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides'((file (styles basic partial-completion)))))

(use-package rust-mode
  :ensure t
  :bind ((:map rust-mode-map
	       ("C-c C-c C-c" . 'rust-compile)
	       ("C-c C-c C-k" . 'rust-check)
	       ("C-c C-c C-t" . 'rust-test)
	       ("C-c C-c C-r" . 'rust-run)))
  :init
  (setq rust-format-on-save t)
  :hook (rust-mode . lsp))

(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-dark t))

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

(use-package web-mode
  :ensure t
  :mode (("\\.svelte?\\'" . web-mode)
	 ("\\.html?\\'" . web-mode)
	 ("\\.js?\\'" . web-mode))
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

(use-package visual-regexp
  :ensure t
  :bind (("C-s" . 'vr/isearch-forward)
	 ("C-r" . 'vr/isearch-backward)
	 ("C-M-r" . 'vr/replace)))

(use-package visual-regexp-steroids
  :after visual-regexp
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))
