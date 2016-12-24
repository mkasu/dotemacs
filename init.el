;;; init.el --- init

;;; Commentary:
;;; Personal config

;;; Needs to come first so the package-archives customization gets
;;; picked up.
(setq custom-file "~/.emacs.d/config/01-custom.el")
(load custom-file)

;; User Info
(setq user-full-name "Marc A. Kastner")
(setq user-mail-address "marc@mkasu.org")

;;; Code:
(require 'package
    (setq package-enable-at-startup nil)
    (setq package-check-signature nil))

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'org)
(org-babel-load-file
 (expand-file-name "settings.org"
                   user-emacs-directory))

;;; init.el ends here
