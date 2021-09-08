(require 'dap-lldb)
(require 'dap-gdb-lldb)

(dap-gdb-lldb-setup)
(dap-register-debug-template
 "Rust::LLDB Run Configuration"
 (list :type "lldb"
       :request "launch"
       :name "GDB::Run"
       :gdbpath "rust-lldb"
       :target nil
       :cwd nil))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm
  (setq-local buffer-save-without-query t))

(setq rustic-format-on-save t)
(add-hook 'rustic-mode-hook 'rk/rustic-mode-hook)

(provide 'setup-rustic-mode)
;;; setup-rustic-mode.el ends here
