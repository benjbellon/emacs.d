(add-hook 'c++-mode-hook
	  (lambda ()
	    (font-lock-add-keywords
	     nil
	     '(;; add some keywords
	       ("\\<\\(nullptr\\)\\>" . font-lock-keyword-face)))))

(add-hook 'c++-mode-hook 'rtags-start-process-unless-running)

(setq c-basic-offset 2)
(setq c-default-style "gnu")

(provide 'setup-c++-mode)
