(require 'markdown-mode)
(setq markdown-imenu-generic-expression
      '(("title"  "^\\(.*\\)[\n]=+$" 1)
	("h2-"    "^\\(.*\\)[\n]-+$" 1)
	("h1"   "^# \\(.*\\)$" 1)
	("h2"   "^## \\(.*\\)$" 1)
	("h3"   "^### \\(.*\\)$" 1)
	("h4"   "^#### \\(.*\\)$" 1)
	("h5"   "^##### \\(.*\\)$" 1)
	("h6"   "^###### \\(.*\\)$" 1)
	("fn"   "^\\[\\^\\(.*\\)\\]" 1)
	))

(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md' ." markdown-mode))

(autoload 'gfm-mode "markdown-mode"
  (add-to-list 'auto-mode-alist '("README\\.md]]'" .gfm-mode)))

(setq markdown-command "pandoc")

(add-hook 'markdown-mode-hook
	  (lambda ()
	    (setq imenu-generic-expression markdown-imenu-generic-expression)))

(provide 'setup-markdown-mode)
