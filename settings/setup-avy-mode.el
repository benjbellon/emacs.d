;;; setup-avy-mode.el --- basic avy mode config      -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Benj Bellon

;; Author: Benj Bellon <bbellon@bbellon-ld2.linkedin.biz>
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; basic config for avy mode

;;; Code:

(require 'avy)

(global-set-key (kbd "C-c k") 'avy-goto-char)

(provide 'setup-avy-mode)
;;; setup-avy-mode.el ends here
