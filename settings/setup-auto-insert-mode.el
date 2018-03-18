(require 'autoinsert)

(defun custom/expand-yasnippet ()
  "Replace with real stuff"
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

(custom-set-variables
 '(auto-insert-directory (locate-user-emacs-file "templates/auto-insert")))

;; Activate globally
(auto-insert-mode)
(setq auto-insert-query nil)

;; Some global auto-insert variables
(setq user-full-name "Benj Bellon"
      user-email "benjaminbellon@gmail.com")

;; C
(define-auto-insert "\\.c\\'"  ["template.c" custom/expand-yasnippet])
(define-auto-insert "\\.h\\'"  ["template.h" custom/expand-yasnippet])

;; C++
(define-auto-insert "\\.cc\\'"  ["template.cc" custom/expand-yasnippet])
(define-auto-insert "\\.hh\\'"  ["template.cc" custom/expand-yasnippet])

;; Python
(define-auto-insert "\\.py\\'"  ["template.py" custom/expand-yasnippet])


(provide 'setup-auto-insert-mode)
