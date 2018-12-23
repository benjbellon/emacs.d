;;; fetch-includes.el --- add headers to clang checker  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Benj Bellon

;; Author: Benj Bellon
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

;; Parses the compile_commands.json db and adds headers
;; to company-mode's company-clang backend.

;;; Code:

(require 'json)
(require 'dash)

(defun get-commands (json-file)
  "Retrieves list of command strings possibly including header paths.
JSON-FILE is the path to the json-file to parse."
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (json (json-read-file json-file)))
    (mapcar
     (function (lambda (el) (gethash "command" el)))
     json)))

(defun get-include-paths (commands-list)
  "Retrieve the include paths from a list of string which may be include paths.
COMMANDS-LIST a list of commands"
  (remove nil
          (mapcar
           (function (lambda (cmd) (if (string-match "-I" cmd) cmd)))
           (split-string commands-list))))

(defun company-clang-header-paths (db-path)
  "Provides list of include paths to add to company-clang backend.
DB-PATH the path to compile_commands.json"
  (-flatten (mapcar
             (function (lambda(cmd-list)
                         (get-include-paths cmd-list)))
             (get-commands db-path))))

(provide 'fetch-includes)
;;; fetch-includes.el ends here
