;;; init.el --- init

;;; Commentary:
;;; Personal config

;; (add-to-list 'load-path "/home1/kastnerm/.emacs.d/elpa/benchmark-init-20150905.238/")
;; (require 'benchmark-init)
;; (benchmark-init/activate)

(require 'org)
;; Open the configuration
(find-file (concat user-emacs-directory "init.org"))
;; tangle it
(org-babel-tangle)
;; load it
(load-file (concat user-emacs-directory "init.el"))
;; finally byte-compile it
(byte-compile-file (concat user-emacs-directory "init.el"))

;;; Code:

;;; init.el ends here
