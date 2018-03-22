(require 'ido)
(require 'flx-ido)
(ido-mode t)
(ido-everywhere t)
(flx-ido-mode t)

;; disable flex matching to use flx-ido
(setq ido-disable-flex-matching t
      ido-use-faces nil
      ido-create-new-buffer 'always)

(require 'ido-vertical-mode)
(ido-vertical-mode)

;; C-n/p is more intuitive in vertical layout
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

(defun my/ido-go-straight-home ()
  (interactive)
  (cond
   ((looking-back "/") (insert "~/"))
   (:else (call-interactively 'self-insert-command))))

(defun my/setup-ido ()
  ;; Go straight home
  (define-key ido-file-completion-map (kbd "~") 'my/ido-go-straight-home)
  (define-key ido-file-completion-map (kbd "C-~") 'my/ido-go-straight-home)

  ;; Use C-w to go back up a dir to better match normal usage of C-w
  ;; - insert current file name with C-x C-w instead.
  (define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-updir)
  (define-key ido-file-completion-map (kbd "C-x C-w") 'ido-copy-current-file-name)

  (define-key ido-file-dir-completion-map (kbd "C-w") 'ido-delete-backward-updir)
  (define-key ido-file-dir-completion-map (kbd "C-x C-w") 'ido-copy-current-file-name))

(add-hook 'ido-setup-hook 'my/setup-ido)

;; Always rescan buffer for imenu
(set-default 'imenu-auto-rescan t)

(add-to-list 'ido-ignore-directories "target")
(add-to-list 'ido-ignore-directories "node_modules")

(provide 'setup-ido)
