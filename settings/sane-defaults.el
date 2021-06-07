;; Stolen from Magnars, with mods
;; https://github.com/magnars/.emacs.d/blob/master/settings/sane-defaults.el

;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; setup eval elisp
(define-key global-map (kbd "C-x C-e") 'eval-last-sexp)

;; define browsers
(define-key global-map (kbd "C-x w") 'browse-url-emacs)
(define-key global-map (kbd "C-x M-w") 'browse-url-chromium)

;; define wrappers
(global-set-key (kbd "M-[") 'insert-pair)
(global-set-key (kbd "M-{") 'insert-pair)
(global-set-key (kbd "M-\"") 'insert-pair)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; dired listing switches
(setq dired-listing-switches "-lisah")

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Transparently open compressed files
(auto-compression-mode t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Don't highlight matches with jump-char - it's distracting
(setq jump-char-lazy-highlight-face nil)

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)

;; Save a list of recent files visited. (open recent file with C-x f)
(recentf-mode 1)
(setq recentf-max-saved-items 100) ;; just 20 is too recent

;; Save minibuffer history
(savehist-mode 1)
(setq history-length 1000)

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;; More memory than even Magnars...'cause the future keeps happening
;; 100 MB should be good
(setq gc-cons-threshold 100000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; 80 chars is a good width.
(set-default 'fill-column 80)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Offer to create parent directories if they do not exist
;; http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun my-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
	       (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions 'my-create-non-existent-directory)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(prefer-coding-system 'utf-8)

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; swap regexp search and regular search bindings
(global-set-key (kbd "C-s") 'vr/isearch-forward)
(global-set-key (kbd "C-r") 'vr/isearch-backward)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; replace en masse
(global-set-key (kbd "S-C-r") 'vr/replace)

;; eval  mode in region buffer
(global-set-key (kbd "C-c C-e") 'eval-last-sexp)

;; undo should not need shift
(global-set-key (kbd "C--") 'undo)

;; compilation commands
(global-set-key (kbd "C-x C-m C-c") 'compile)
(global-set-key (kbd "C-x C-m C-m") 'recompile)


;; copy/paste commands for linux
(global-set-key (kbd "s-c") 'clipboard-kill-region)
(global-set-key (kbd "s-v") 'clipboard-yank)

;; emojify everywhere!
(global-emojify-mode 1)

(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
	(e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
			     "python -mjson.tool" (current-buffer) t)))

;; Dired Settings
(setq dired-omit-mode t)

;; which-key everywhere
(setq which-key-mode t)

;; Override C-x C-c to open the default ansi-term buffer
;; 0. check if we are in GUI or user want to override behavior
;; 1. check if ansi-term buffer exists
;; 2. load that buffer if it does exist
;; 3. create and load that buffer if it does not, and delete other windows
(if (display-graphic-p)
    (let ((ansi-buffer "*ansi-term*")
	  (quit-command "C-x C-c"))
      (if (not (get-buffer ansi-buffer))
	  (ansi-term "/bin/bash"))
      (define-key global-map (kbd quit-command)
	(lambda () (interactive)
	  (delete-other-windows)
	  (switch-to-buffer "*ansi-term*")))))

(provide 'sane-defaults)
;;; sane-defaults.el ends here
