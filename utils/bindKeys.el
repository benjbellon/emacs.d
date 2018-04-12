;;; bindKeys.el --- bind key sequence to a key map   -*- lexical-binding: t; -*-

;; Author: Benj Bellon
;; Keywords: lisp

;;; Commentary:

;; Bind key seqeuence to a keymap

;;; Code:
(require 'dash)

(defun bindKeys (triggerSeq bindings)
  "Bind keys to a trigger sequence.
TRIGGERSEQ is a sequence of key(s) to trigger the binding.
BINDINGS is an alist of bindings."
  (let ((trigger (concat (-reduce (lambda (acc cur) (concat acc cur)) triggerSeq) " ")))
    (mapcar (lambda (binding)
              (global-set-key
               (kbd (concat (concat "C-c " trigger) (car binding)))
               (cdr binding)))
            bindings)))

(provide 'bindKeys)
;;; bindKeys.el ends here
