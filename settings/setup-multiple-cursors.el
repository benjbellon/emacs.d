;;; setup-multiple-cursors.el --- multiple cursors config  -*- lexical-binding: t; -*-

;;; Commentary:

;; basic multiple cursors config

;;; Code:
(require 'bindKeys)


(bindKeys '("m")
          '(("m" . mc/mark-all-like-this)
            ("l" . mc/edit-lines)
            ("n" . mc/mark-next-like-this)
            ("p" . mc/mark-previous-like-this)
            ("r r" . mc/mark-all-in-region-regexp)
            ("r v" . vr/mc-mark)
            ("o" . mc/mark-pop)
            ("s" . mc/sort-regions)
            ("i" . mc/insert-numbers)))


(provide 'setup-multiple-cursors)
;;; setup-multiple-cursors.el ends here
