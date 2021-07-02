(require 'clojure-mode)
(require 'clojure-mode-extra-font-locking)
(require 'flycheck-clojure)

(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojurescript-mode-hook 'lsp)
(add-hook 'clojurec-mode-hook 'lsp)

(add-hook 'clojure-mode-hook 'enable-paredit-mode)
(add-hook 'clojure-mode-hook 'prettify-symbols-mode)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-minimum-prefix-length 1
      lsp-lens-enable t
      lsp-signature-auto-activate nil
                                        ; lsp-enable-indentation nil ; uncomment to use cider indentation instead of lsp
                                        ; lsp-enable-completion-at-point nil ; uncomment to use cider completion instead of lsp
      )


(provide 'setup-clojure-mode)
