(require 'dap-lldb)
(require 'dap-gdb-lldb)
;; installs .extension/vscode
(dap-gdb-lldb-setup)
(dap-register-debug-template
 "Rust::LLDB"
 (list :type "lldb"
       :request "launch"
       :name "LLDB::Run"
       :gdbpath "rust-lldb"
       :target nil
       :cwd (projectile-project-root)))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm
  (setq-local buffer-save-without-query t))

(setq rustic-format-on-save t)
(setq lsp-rust-analyzer-server-display-inlay-hints t)
(add-hook 'rustic-mode-hook 'rk/rustic-mode-hook)

(provide 'setup-rustic-mode)
;;; setup-rustic-mode.el ends here
