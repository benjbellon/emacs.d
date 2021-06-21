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
;; Suppress splash screen

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))
(setq create-lockfiles nil)
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq inhibit-startup-message t)

(add-to-list 'load-path (expand-file-name "settings" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "utils" user-emacs-directory))

(require 'setup-package)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
		 (concat user-emacs-directory "backups")))))

(defun init-install-packages()
  (packages-install
   '(ace-window
     blacken
     cargo
     cider
     company
     company-c-headers
     company-glsl
     company-terraform
     clojure-mode
     clojure-mode-extra-font-locking
     clojure-snippets
     cmake-mode
     cquery
     dash
     dap-mode
     dart-mode
     dockerfile-mode
     elm-mode
     emojify
     erlang
     exec-path-from-shell
     flutter
     flx-ido
     flycheck
     flycheck-clangcheck
     flycheck-clojure
     flycheck-haskell
     flycheck-pos-tip
     go-mode
     haskell-mode
     hasklig-mode
     highlight-escape-sequences
     ido-vertical-mode
     json
     lsp-dart
     lsp-mode
     lsp-ui
     magit
     markdown-mode
     modern-cpp-font-lock
     multiple-cursors
     paredit
     play-routes-mode
     projectile
     projectile-ripgrep
     protobuf-mode
     purescript-mode
     pyvenv-mode
     org-bullets
     restclient
     rustic
     rust-playground
     sbt-mode
     slime
     smart-mode-line
     solarized-theme
     swift-mode
     swift-playground-mode
     systemd
     terraform-mode
     treemacs
     treemacs-projectile
     tuareg
     typescript-mode
     uuidgen
     visual-regexp
     visual-regexp-steroids
     web-mode
     which-key
     yaml-mode
     yasnippet)))

(condition-case nil
    (init-install-packages)
  (error
   (package-refresh-contents)
   (init-install-packages)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Are we in gui or terminal?
(if (display-graphic-p)
    ;; load the theme so we don't have a block of white for too long upon startup
    (load-theme 'solarized-dark t))

(require 'visual-regexp)
(require 'visual-regexp-steroids)

(require 'sane-defaults)

(when is-mac
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(add-to-list 'exec-path "/usr/local/bin")

(require 'setup-ace-window-mode)
(require 'setup-auto-insert-mode)
(require 'setup-avy-mode)
(require 'setup-c++-mode)
(require 'setup-clojure-mode)
(require 'setup-company-mode)
(require 'setup-dockerfile-mode)
(require 'setup-flycheck)
(require 'setup-go-mode)
(require 'setup-haskell-mode)
(require 'setup-ido)
(require 'setup-keychain-environment)
(require 'setup-lisp-mode)
(require 'setup-lsp-mode)
(require 'setup-magit)
(require 'setup-markdown-mode)
(require 'setup-multiple-cursors)
(require 'setup-org-mode)
(require 'setup-plz-mode)
(require 'setup-projectile-mode)
(require 'setup-protobuf-mode)
(require 'setup-purescript-mode)
(require 'setup-python-mode)
(require 'setup-rustic-mode)
(require 'setup-slime-mode)
(require 'setup-terraform-mode)
(require 'setup-treemacs)
(require 'setup-typescript-mode)
(require 'setup-txt-mode)
(require 'setup-web-mode)
(require 'setup-yaml-mode)
(require 'setup-yasnippet)


;; utility globals
(require 'fetch-includes)
(exec-path-from-shell-initialize)

;; full power
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(keychain-refresh-environment)
;;; init.el ends here
