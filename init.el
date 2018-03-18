;;; init.el --- initialize emacs configuration

;;; Author: Benj Bellon <benjaminbellon@gmail.com>
;;; Maintainer: Benj Bellon <benjaminbellon@gmail.com>
;;; Homepage: https://github.com/benjbellon/emacs.d
;;; Keywords: emacs, emacs.d, config

;;; Commentary:

;;; This config builds an instance of Emacs personalized for its author.
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
(package-initialize)

;; Suppress splash screen
(setq inhibit-startup-message t)

(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))
(add-to-list 'load-path settings-dir)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
		 (concat user-emacs-directory "backups")))))

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;; Are we in gui or terminal?
(if (display-graphic-p)
    (load-theme 'solarized-dark))


(require 'setup-package)

(defun init-install-packages()
  (packages-install
   '(cider
     clojure-mode
     clojure-mode-extra-font-locking
     clojure-snippets
     dash
     elm-mode
     ember-mode
     ensime
     find-file-in-project
     flycheck
     flycheck-clojure
     flycheck-haskell
     flycheck-pos-tip
     haskell-mode
     highlight-escape-sequences
     ido-vertical-mode
     magit
     markdown-mode
     multiple-cursors
     paredit
     play-routes-mode
     purescript-mode
     org-bullets
     sbt-mode
     scala-mode
     slime
     visual-regexp
     web-mode
     yaml-mode
     yasnippet)))

(condition-case nil
    (init-install-packages)
  (error
   (package-refresh-contents)
   (init-install-packages)))

(require 'sane-defaults)

(when is-mac
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(add-to-list 'exec-path "/usr/local/bin")

(require 'setup-auto-insert-mode)
(require 'setup-c++-mode)
(require 'setup-clojure-mode)
(require 'setup-ember-mode)
(require 'setup-find-file-in-project)
(require 'setup-flycheck)
(require 'setup-haskell-mode)
(require 'setup-ido)
(require 'setup-keychain-environment)
(require 'setup-lisp-mode)
(require 'setup-magit)
(require 'setup-markdown-mode)
(require 'setup-multiple-cursors)
(require 'setup-org-mode)
(require 'setup-purescript-mode)
(require 'setup-scala-mode)
(require 'setup-slime-mode)
(require 'setup-web-mode)
(require 'setup-yaml-mode)
(require 'setup-yasnippet)

(require 'visual-regexp)
(define-key global-map (kbd "M-&") 'vr/query-replace)
(define-key global-map (kbd "M-/") 'vr/replace)

;; full power
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(keychain-refresh-environment)
;;; init.el ends here
