;;; setup-package.el --- initialize package repos

;;; Commentary:

;;; basic m/elpa configuration

;;;Code:
(require 'package)
;; Taken from magnars setup-package.el
;; https://github.com/magnars/.emacs.d/blob/master/settings/setup-package.el
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)

(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa")
  (package-refresh-contents))

;; install dash once if it's not already installed
(when (not (package-installed-p 'dash)) (package-install 'dash))
(require 'dash)
(defun packages-install (packages)
  (--each packages
	  (when (not (package-installed-p it))
	    (package-install it)))
  (delete-other-windows))

(defun require-package (package &optional min-version no-refresh)
    "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
    (if (package-installed-p package min-version)
	t
      (if (or (assoc package package-archive-contents) no-refresh)
	  (package-install package)
	(progn
	  (package-refresh-contents)
	  (require-package package min-version t)))))

(provide 'setup-package)
;;; setup-package.el ends here
