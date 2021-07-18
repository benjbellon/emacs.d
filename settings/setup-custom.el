;;; setup-custom.el --- non-user specific customizations  -*- lexical-binding: t; -*-
(set-face-attribute 'default nil :font "Source Code Pro Medium-9")
(set-fontset-font t nil "JoyPixels" nil 'append)

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
