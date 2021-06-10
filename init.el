(defun make-obsolete (obsolete-name current-name &optional when)
  "Make the byte-compiler warn that function OBSOLETE-NAME is obsolete.
OBSOLETE-NAME should be a function name or macro name (a symbol).

The warning will say that CURRENT-NAME should be used instead.
If CURRENT-NAME is a string, that is the `use instead' message
\(it should end with a period, and not start with a capital).
WHEN should be a string indicating when the function
was first made obsolete, for example a date or a release number."
  (declare (advertised-calling-convention
            ;; New code should always provide the `when' argument.
            (obsolete-name current-name when) "23.1"))
  (put obsolete-name 'byte-obsolete-info
       ;; The second entry used to hold the `byte-compile' handler, but
       ;; is not used any more nowadays.
       (purecopy (list current-name nil when)))
  obsolete-name)

(defmacro define-obsolete-function-alias (obsolete-name current-name
                                                        &optional when docstring)
  "Set OBSOLETE-NAME's function definition to CURRENT-NAME and mark it obsolete.

\(define-obsolete-function-alias \\='old-fun \\='new-fun \"22.1\" \"old-fun's doc.\")

is equivalent to the following two lines of code:

\(defalias \\='old-fun \\='new-fun \"old-fun's doc.\")
\(make-obsolete \\='old-fun \\='new-fun \"22.1\")

WHEN should be a string indicating when the function was first
made obsolete, for example a date or a release number.

See the docstrings of `defalias' and `make-obsolete' for more details."
  (declare (doc-string 4)
           (advertised-calling-convention
            ;; New code should always provide the `when' argument.
            (obsolete-name current-name when &optional docstring) "23.1"))
  `(progn
     (defalias ,obsolete-name ,current-name ,docstring)
     (make-obsolete ,obsolete-name ,current-name ,when)))

(defun make-obsolete-variable (obsolete-name current-name &optional when access-type)
  "Make the byte-compiler warn that OBSOLETE-NAME is obsolete.
The warning will say that CURRENT-NAME should be used instead.
If CURRENT-NAME is a string, that is the `use instead' message.
WHEN should be a string indicating when the variable
was first made obsolete, for example a date or a release number.
ACCESS-TYPE if non-nil should specify the kind of access that will trigger
  obsolescence warnings; it can be either `get' or `set'."
  (declare (advertised-calling-convention
            ;; New code should always provide the `when' argument.
            (obsolete-name current-name when &optional access-type) "23.1"))
  (put obsolete-name 'byte-obsolete-variable
       (purecopy (list current-name access-type when)))
  obsolete-name)

(defmacro define-obsolete-variable-alias (obsolete-name current-name
						        &optional when docstring)
  "Make OBSOLETE-NAME a variable alias for CURRENT-NAME and mark it obsolete.
This uses `defvaralias' and `make-obsolete-variable' (which see).
See the Info node `(elisp)Variable Aliases' for more details.

If CURRENT-NAME is a defcustom or a defvar (more generally, any variable
where OBSOLETE-NAME may be set, e.g. in an init file, before the
alias is defined), then the define-obsolete-variable-alias
statement should be evaluated before the defcustom, if user
customizations are to be respected.  The simplest way to achieve
this is to place the alias statement before the defcustom (this
is not necessary for aliases that are autoloaded, or in files
dumped with Emacs).  This is so that any user customizations are
applied before the defcustom tries to initialize the
variable (this is due to the way `defvaralias' works).

WHEN should be a string indicating when the variable was first
made obsolete, for example a date or a release number.

For the benefit of Customize, if OBSOLETE-NAME has
any of the following properties, they are copied to
CURRENT-NAME, if it does not already have them:
`saved-value', `saved-variable-comment'."
  (declare (doc-string 4)
           (advertised-calling-convention
            ;; New code should always provide the `when' argument.
            (obsolete-name current-name when &optional docstring) "23.1"))
  `(progn
     (defvaralias ,obsolete-name ,current-name ,docstring)
     ;; See Bug#4706.
     (dolist (prop '(saved-value saved-variable-comment))
       (and (get ,obsolete-name prop)
            (null (get ,current-name prop))
            (put ,current-name prop (get ,obsolete-name prop))))
     (make-obsolete-variable ,obsolete-name ,current-name ,when)))

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
     company-lsp
     company-terraform
     clojure-mode
     clojure-mode-extra-font-locking
     clojure-snippets
     cmake-mode
     cquery
     dash
     dap-mode
     dockerfile-mode
     elm-mode
     ember-mode
     erlang
     exec-path-from-shell
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
     org-bullets
     restclient
     rust-mode
     rust-playground
     sbt-mode
     slime
     smart-mode-line
     solarized-theme
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
(require 'setup-ember-mode)
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
(require 'setup-rust-mode)
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
