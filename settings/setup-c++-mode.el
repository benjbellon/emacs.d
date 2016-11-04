(add-hook 'c++-mode-hook
	  (lambda ()
	    (font-lock-add-keywords
	     nil
	     '(;; add some keywords
	       ("\\<\\(nullptr\\)\\>" . font-lock-keyword-face)))))

(provide 'setup-c++-mode)
