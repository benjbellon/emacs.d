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

  (add-to-list 'load-path templates-dir)

  (load custom-file)

  :bind (
	 ("C-x C-b" . ibuffer)
	 ("C--" . undo)
	 ("C-x C-d" . find-file))
  :hook ((prog-mode-hook . 'hl-todo-mode))

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
		      (switch-to-buffer "*ansi-term*"))))))

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
  :bind (
	 ("C-c l f" . consult-focus-lines)
	 ("C-c l s" . consult-ripgrep))  
  :hook (completion-list-mode . consult-preview-at-point-mode))

(use-package embark
  :ensure t
  :bind (("C-c e ." . embark-act)))

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package keychain-environment
  :ensure t
  :config
  (keychain-refresh-environment))

(use-package flycheck
  :ensure t)

(use-package lsp-mode
  :ensure t
  :commands lsp)

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package multiple-cursors
  :ensure t
  :init
  (setq mc/always-run-for-all t)
  :bind (("C-c m i" . mc/insert-numbers)
	 ("C-c m l" . mc/edit-lines)    
	 ("C-c m m" . mc/mark-all-like-this)
	 ("C-c m n" . mc/mark-next-like-this)
	 ("C-c m p" . mc/mark-previous-like-this)    
	 ("C-c m r r" . mc/mark-all-in-region-regexp)))

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

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides'((file (styles basic partial-completion)))))

(use-package rust-mode
  :ensure t
  :bind ((:map rust-mode-map
	       ("C-c C-c C-c" . rust-compile)
	       ("C-c C-c C-k" . rust-check)
	       ("C-c C-c C-t" . rust-test)
	       ("C-c C-c C-r" . rust-run)))
  :init
  (setq rust-format-on-save t)  
  :hook (rust-mode . lsp))

(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-dark t))

(use-package tree-sitter
  :init
  (setq treesit-language-source-alist
	'((bash "https://github.com/tree-sitter/tree-sitter-bash")
	  (cmake "https://github.com/uyha/tree-sitter-cmake")
	  (css "https://github.com/tree-sitter/tree-sitter-css")
	  (elisp "https://github.com/Wilfred/tree-sitter-elisp")
	  (go "https://github.com/tree-sitter/tree-sitter-go")
	  (haskell "https://github.com/tree-sitter/tree-sitter-haskell.git")
	  (html "https://github.com/tree-sitter/tree-sitter-html")
	  (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	  (json "https://github.com/tree-sitter/tree-sitter-json")
	  (make "https://github.com/alemuller/tree-sitter-make")
	  (markdown "https://github.com/ikatyang/tree-sitter-markdown")
	  (python "https://github.com/tree-sitter/tree-sitter-python")
	  (rust "https://github.com/tree-sitter/tree-sitter-rust.git")
	  (toml "https://github.com/tree-sitter/tree-sitter-toml")
	  (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	  (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	  (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  ;; TODO: install if not exists...
  :hook (tree-sitter-after-on-hook . tree-sitter-hl-mode))

(use-package undo-tree
  :ensure t)

(use-package vertico
  :ensure t
  :bind (:map vertico-map
	      ("C-d" . 'vertico-exit)
	      ("DEL" . 'vertico-directory-delete-char))
  :init
  (vertico-mode)
  :hook (rfn-eshadow-update-overlay-hook . vertico-directory-tidy)
  :custom
  (vertico-count 25)
  (vertico-resize t))

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))
