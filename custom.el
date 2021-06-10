;; reserved for all local customization added by emacs
;; do not modify
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-insert-directory (locate-user-emacs-file "templates/auto-insert"))
 '(c-default-style "linux")
 '(custom-safe-themes
   '("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))
 '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)
 '(package-selected-packages
   '(lsp-dart dart-server dart-mode flutter emojify auctex flycheck-aspell elixir-mode add-node-modules-path prettier-js prettier hasklig-mode jq-mode uuidgen restclient erlang rust-mode flymake-go go-mode gradle-mode treemacs-projectile treemacs protobuf-mode yaml-mode web-mode visual-regexp-steroids systemd solarized-theme smart-mode-line slime purescript-mode projectile-ripgrep play-routes-mode paredit org-bullets multiple-cursors modern-cpp-font-lock magit lsp-ui ido-vertical-mode highlight-escape-sequences flycheck-pos-tip flycheck-haskell flycheck-clojure flycheck-clangcheck flx-ido ensime ember-mode elm-mode dockerfile-mode cquery company-lsp company-glsl company-c-headers cmake-mode clojure-snippets clojure-mode-extra-font-locking ace-window))
 '(safe-local-variable-values '((projectile-project-run-cmd . "./build/jmessageGtk"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 90 :width normal :family "Source Code Pro Medium"))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.0))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.0))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))
(require 'setup-custom)
(provide 'custom)
;;; custom.el ends here
