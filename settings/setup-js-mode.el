(require 'js2-mode)

(add-hook 'js2-mode-hook 'ac-js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(custom-set-variables
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t))

(provide 'setup-js-mode)
