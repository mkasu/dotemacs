;;; 30-org.el --- Org Mode

;;; Commentary:
;; Org Mode configuration


;;; Code:

(require 'use-package)

(defun sacha/org-agenda-skip-scheduled ()
  (org-agenda-skip-entry-if 'scheduled 'deadline 'regexp "\n]+>"))

(use-package org
  :ensure t
  :bind ("C-c a" . org-agenda)
  :init
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook 'flyspell-mode)
  ;; Agenda
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-custom-commands
        '(("h" "Main view"
           ((agenda "" ((org-agenda-ndays 7)                      ;; overview of appointments
                        (org-agenda-log-mode 1)
                        (org-agenda-start-on-weekday nil)         ;; calendar begins today
                        (org-agenda-repeating-timestamp-show-all t)
                        )
                    )         
           (alltodo ""
                    ((org-agenda-skip-function 'sacha/org-agenda-skip-scheduled)
                     (org-agenda-overriding-header "Unscheduled TODO entries: ")
                     )
                    )
           )
           )
          )
        )
  (setq org-agenda-files (quote ("~/org")))
  (setq org-log-done 'time)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
  ;; Run/highlight code using babel in org-mode
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
