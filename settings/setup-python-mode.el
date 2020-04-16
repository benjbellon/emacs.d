(require 'blacken)

(add-hook 'python-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'blacken-buffer)))

(provide 'setup-python-mode)
