(require 'clojure-mode)
(require 'clojure-mode-extra-font-locking)
(require 'flycheck-clojure)

(add-hook 'clojure-mode-hook 'enable-paredit-mode)
(add-hook 'clojure-mode-hook 'prettify-symbols-mode)

(provide 'setup-clojure-mode)
