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
(setq package-enable-at-startup nil))

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
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

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; === Basics ===
(load-file "~/.emacs.d/config/10-look.el")
(load-file "~/.emacs.d/config/11-behave.el")
(load-file "~/.emacs.d/config/12-utf8.el")
(load-file "~/.emacs.d/config/13-jp.el")
;; (load-file "~/.emacs.d/config/15-evil.el")
(load-file "~/.emacs.d/config/19-mail.el")

;; === Documents ===
(load-file "~/.emacs.d/config/30-org.el")
(load-file "~/.emacs.d/config/31-latex.el")
(load-file "~/.emacs.d/config/32-other.el")

;; === Programming ===
(load-file "~/.emacs.d/config/40-prog-gen.el")
(load-file "~/.emacs.d/config/41-prog-cpp.el")
(load-file "~/.emacs.d/config/42-prog-python.el")
(load-file "~/.emacs.d/config/43-prog-ruby.el")

;;; init.el ends here
