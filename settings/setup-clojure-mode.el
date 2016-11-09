(require 'clojure-mode)
(require 'clojure-mode-extra-font-locking)
(require 'flycheck-clojure)

(add-hook 'clojure-mode-hook 'enable-paredit-mode)
(add-hook 'clojure-mode-hook 'prettify-symbols-mode)

;; This should be in it's own setup file
(add-to-list 'auto-mode-alist '("\\.el\\'" . paredit-mode))

(provide 'setup-clojure-mode)
