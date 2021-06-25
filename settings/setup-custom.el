;;; setup-custom.el --- non-user specific customizations  -*- lexical-binding: t; -*-

(set-frame-font "Source Code Pro Medium" nil t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 90 :width normal :family "Source Code Pro Medium"))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.0))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.0))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))

;; no line highlight
(hl-line-mode 0)

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(require 'saveplace)
(setq-default save-place t)

(setq-default indent-tabs-mode nil)

(show-paren-mode 1)

(setq sml/theme 'dark)
(sml/setup)

(provide 'setup-custom)
;;; setup-custom.el ends here
