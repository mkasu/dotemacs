;;; 30-org.el --- Org Mode

;;; Commentary:
;; Org Mode configuration


;;; Code:

(use-package org
  :ensure t
  :init
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook 'flyspell-mode)
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-files (quote ("~/org")))
  (setq org-log-done 'time)
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

(use-package org-projectile
  :bind (("C-c n p" . org-projectile:project-todo-completing-read)
         ("C-c n c" . org-capture))
  :config
  (progn
    (setq org-projectile:projects-file 
          "~/org/projects.org")
    (setq org-agenda-files (append org-agenda-files (org-projectile:todo-files)))
    (add-to-list 'org-capture-templates (org-projectile:project-todo-entry "p"))
    (add-to-list 'org-capture-templates (org-projectile:project-todo-entry "l" "* TODO %? %a\n" "Linked Project TODO"))
    )
  :ensure t)

;;; 30-org.el ends here
