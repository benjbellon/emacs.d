(require 'flycheck)
(require 'flycheck-pos-tip)

(global-flycheck-mode 1)

(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(provide 'setup-flycheck)