;;; 32-other.el --- Other normal files

;;; Commentary:
;; Tools for working with normal files

;;; Code:

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . markdown-mode)
  	 ("\\.mdwn\\'" . markdown-mode)
  	 ("\\.markdown\\'" . markdown-mode))
  )

;;; 32-other.el ends here
