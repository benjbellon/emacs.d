;;; .yas-setup.el --- Utility functions for c-mode   -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Benj Bellon

;; Author: Benj Bellon <benj@benj-ad1>
;; Keywords: c

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

;; As description

;;; Code:
(require 'dash)

(defun fetch-project-path (root-dir)
  "Retrieve path from project's root-dir to file as list.
NOTE: The closest parent root-dir is considered the root.
ROOT-DIR: the root directory with no path delimiters."
  (let* ((full-path (locate-dominating-file buffer-file-name root-dir)))
    (if full-path
	(let* ((proj-root-dir (nth 1 (reverse (split-string full-path "/"))))
	      (full-path-list (cons proj-root-dir
				    (-flatten (cdr (-split-on proj-root-dir (split-string buffer-file-name "/")))))))

	  ;; turn dashes into underscores
	  (mapcar (lambda (x) (replace-regexp-in-string "-" "_" x))
		  full-path-list))
      nil)
    ))

(defun build-include-guard-header (root-dir)
  "Create a full include guard header for c files.
If the path cannot be found, just use the filename.
ROOT-DIR: the root directory with no path delimiters.
FORMAT: PROJDIR_FULL_PATH_TO_FILE_H"
  (if (fetch-project-path root-dir)
      (let* ((path-no-file (reverse (cdr (reverse (fetch-project-path root-dir)))))
	     (file-no-ext (car (split-string (car (reverse (fetch-project-path root-dir))) "\\." t))))
	(concat
	 (mapconcat 'identity
		    (mapcar 'upcase
			    (reverse (cons file-no-ext (reverse path-no-file)))) "_")
	 "_H")
	)
    ;; no lazy evaluation, so just do it here
    (concat
     (upcase
      (car (-filter (lambda (x) (not (s-blank? x)))
		    (split-string
		     (car (reverse (split-string buffer-file-name "/")))
		     "\\."))))
     "_H")
    ))

(defun git-include-guard-path ()
  "Build include guard headers for git projects."
  (build-include-guard-header ".git"))

(provide '.yas-setup)
;;; .yas-setup.el ends here
