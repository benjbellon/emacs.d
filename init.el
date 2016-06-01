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

(require 'setup-package)

(defun init--install-packages()
  (packages-install
   '(cider
     clojure-mode
     clojure-mode-extra-font-locking
     clojure-snippets
     dash
     flycheck
     flycheck-clojure
     flycheck-haskell
     flycheck-pos-tip
     haskell-mode
     highlight-escape-sequences
     ido-vertical-mode
     markdown-mode
     multiple-cursors
     paredit
     scala-mode
     visual-regexp
     yasnippet)))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

(require 'sane-defaults)

(when is-mac
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(require 'setup-clojure-mode)
(require 'setup-flycheck)
(require 'setup-haskell-mode)
(require 'setup-ido)
(require 'setup-markdown-mode)
(require 'setup-multiple-cursors)
(require 'setup-yasnippet)

(require 'visual-regexp)
(define-key global-map (kbd "M-&") 'vr/query-replace)
(define-key global-map (kbd "M-/") 'vr/replace)

;; full power
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
