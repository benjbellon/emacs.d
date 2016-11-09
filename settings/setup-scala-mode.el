(require 'scala-mode)
(require 'ensime)

(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(provide 'setup-scala-mode)

