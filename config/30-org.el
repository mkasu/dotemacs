;;; 30-org.el --- Org Mode

;;; Commentary:
;; Org Mode configuration


;;; Code:

(use-package org
  :init
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook 'flyspell-mode)
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-files (quote ("~/org")))
  ; Run/highlight code using babel in org-mode
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (python . t)
     (C . t)
     ))
  ;; Syntax hilight in #+begin_src blocks
  (setq org-src-fontify-natively t)
  :defer t
  )

;;; 30-org.el ends here
