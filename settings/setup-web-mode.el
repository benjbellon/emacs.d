(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.eex\\'" . web-mode))


(set-face-attribute 'web-mode-doctype-face nil :foreground "misty rose")
(set-face-attribute 'web-mode-html-tag-face nil :foreground "medium slate blue")
(set-face-attribute 'web-mode-html-attr-name-face nil :foreground "orange")
(set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "gray")
(set-face-attribute 'web-mode-block-control-face nil :foreground "deep sky blue")
(set-face-attribute 'web-mode-block-delimiter-face nil :foreground "medium aquamarine")

(setq web-mode-engines-alist
      '(("jinja" . "\\.html\\'")))

(defun indent-offset ()
  "Hooks for web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-style-padding 2)
  (setq web-mode-script-padding 2))

;; (eval-after-load 'web-mode
;;   '(add-hook 'web-mode-hook #'add-node-modules-path))

(add-hook 'web-mode-hook 'indent-offset)
;(add-hook 'web-mode-hook 'prettier-js-mode)

(provide 'setup-web-mode)
