(require 'haskell-mode)
(require 'lsp-haskell)
(require 'flycheck-haskell)

(defun haskell/pretty-symbols ()
  (setq prettify-symbols-alist
        '(
          ("\\" . ?λ)
          (">=" . ?≥)
          ("<=" . ?≤)
          ("()" . ?∅))))

;; (add-hook 'haskell-mode-hook 'hasklig-mode)
;; (add-hook 'haskell-mode-hook 'haskell/pretty-symbols)

;; (setq inferior-haskell-find-project-root nil)

(provide 'setup-haskell-mode)
