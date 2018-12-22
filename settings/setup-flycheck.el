(require 'flycheck)
(require 'flycheck-pos-tip)
(require 'flycheck-clangcheck)

;; When using flycheck in a larger c++ project with a variety of headers
;; across a variety of dirs, add the following to your .dir-locals.el
;; (flycheck-clang-dbname . "/abs/path/to/build/compile_commands.json")

(defun setup-custom-clang-checker ()
  (flycheck-set-checker-executable 'c/c++-clangcheck "/usr/bin/clang-check")
  (flycheck-select-checker 'c/c++-clangcheck))

(add-hook 'c-mode-hook #'setup-custom-clang-checker)
(add-hook 'c++-mode-hook #'setup-custom-clang-checker)

(global-flycheck-mode 1)

(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(global-set-key (kbd "<f2>") 'flyspell-auto-correct-previous-word)
(setq flycheck-clangcheck-analyze t)

(provide 'setup-flycheck)
