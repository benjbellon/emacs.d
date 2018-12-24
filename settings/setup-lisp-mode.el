;;; setup-lisp-mode.el --- lisp mode settings        -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Benj Bellon

;; Author: Benj Bellon <benj@benj-ad1>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Basic settings forlisp mode

;;; Code:

(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

(slime-mode)

;; setup eval inferior lisp in slime
;; (define-key global-map (kbd "C-c C-e") 'slime-eval-last-expression)

(provide 'setup-lisp-mode)
;;; setup-lisp-mode.el ends here
