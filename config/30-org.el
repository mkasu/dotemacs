;;; 30-org.el --- Org Mode

;;; Commentary:
;; Org Mode configuration


;;; Code:

(require 'use-package)

(use-package org
  :ensure t
  :bind ("C-c a" . org-agenda)
  :init
  (progn
    (add-hook 'org-mode-hook 'visual-line-mode)
    (add-hook 'org-mode-hook 'org-indent-mode)
    (add-hook 'org-mode-hook 'flyspell-mode)
    ;; Agenda
    (setq org-agenda-window-setup 'current-window)
    
    (setq org-agenda-overriding-columns-format "%CATEGORY %50ITEM %SCHEDULED %DEADLINE")
    (setq org-agenda-custom-commands
          '(("H" "Detailed view"
             ((agenda "" ((org-agenda-ndays 7)                      ;; overview of appointments
                          (org-agenda-log-mode 1)
                          (org-agenda-start-on-weekday nil)         ;; calendar begins today
                          (org-agenda-repeating-timestamp-show-all t)
                          )
                      )         
              (alltodo ""
                       ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline 'regexp "\n]+>"))
                        (org-agenda-overriding-header "Unscheduled TODO entries:")
                        )
                       )
              (tags-todo "SCHEDULED>\"<+1w>\""
                         ((org-agenda-overriding-header "Future TODO entries:")
                          )
                         )
              (tags-todo "DEADLINE>\"<+1w>\""
                         ((org-agenda-overriding-header "Future Deadlines:")
                          )
                         )
              )
             )
            ("h" "Main view"
             ((agenda "" ((org-agenda-ndays 7)                      ;; overview of appointments
                          (org-agenda-log-mode 1)
                          (org-agenda-start-on-weekday nil)         ;; calendar begins today
                          (org-agenda-repeating-timestamp-show-all t)
                          (org-agenda-use-time-grid t)))       
              (alltodo ""
                       ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline 'regexp "\n]+>"))
                        (org-agenda-overriding-header "Unscheduled TODO entries:")
                        )
                       )
              )
             )
            )
          )
    (setq org-agenda-files (quote ("~/Dropbox/org")))
    (setq org-log-done 'time)
    (setq org-agenda-skip-deadline-if-done t)
    (setq org-agenda-skip-scheduled-if-done t)
    (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
    (setq org-support-shift-select t)
    (setq org-agenda-show-inherited-tags 'nil)
    (setq org-publish-project-alist
          '(("html"
             :base-directory "~/Dropbox/org/"
             :base-extension "org"
             :publishing-directory "~/Dropbox/org/exports"
             :publishing-function org-html-publish-to-html)
            ("pdf"
             :base-directory "~/Dropbox/org/"
             :base-extension "org"
             :publishing-directory "~/Dropbox/org/exports"
             :publishing-function org-latex-publish-to-pdf)
            ("all" :components ("html" "pdf"))))
    (setq org-latex-pdf-process
          '("latexmk -xelatex -shell-escape -interaction=nonstopmode -output-directory=%o %f"))
    (defun my-org-mode-hook()
      (progn
        (auto-fill-mode 1)))
    (add-hook 'org-mode-hook 'my-org-mode-hook)
    ;; Run/highlight code using babel in org-mode
    (org-babel-do-load-languages
     'org-babel-load-languages
     '(
       (python . t)
       (C . t)
       ))
    ;; Syntax hilight in #+begin_src blocks
    (setq org-src-fontify-natively t)
    ;; Capturing
    (setq org-capture-templates
          '(
            ("t" "Tasks" entry
             (file+headline "~/Dropbox/org/todo.org" "Inbox")
             "* TODO %^{Task}
SCHEDULED: %^t
%<%Y-%m-%d %H:%M>
%?
")
            ("a" "Appointment" entry
             (file+headline "~/Dropbox/org/calendar.org" "Inbox")
             "* %^{Appointment}
%^t
%?
")
            ("T" "Quick task" entry
             (file+headline "~/Dropbox/org/todo.org" "Inbox")
             "* TODO %^{Task}"
             :immediate-finish t)
            ("B" "Book" entry
             (file+headline "~/Dropbox/org/books.org" "Inbox")
             "* %^{Title}  %^g
%i
*Author(s):* %^{Author}

%?

%U"
             )
            ("j" "Japanese Grammar" entry
             (file+headline "~/Dropbox/org/japanese.org" "Grammar")
             "* %^{Title}

%?

例文
- 
"
             )
            )
          )
    (setq org-icalendar-timezone "Europe/Berlin")
    (setq org-icalendar-use-deadline '(event-if-todo event-if-not-todo todo-due))
    (setq org-icalendar-combined-agenda-file "~/Dropbox/org/agenda.ics")

    ;; async export
    (setq org-export-async-debug nil)
    (setq org-export-in-background t)
    
    ;; Org Projectile
    (use-package org-projectile
      :bind (("C-c n p" . org-projectile:project-todo-completing-read)
             ("C-c n c" . org-capture))
      :ensure t
      :demand t
      :config
      (setq org-projectile:projects-file "~/Dropbox/org/projects.org")
      (add-to-list 'org-capture-templates (org-projectile:project-todo-entry "p"))
      (add-to-list 'org-capture-templates (org-projectile:project-todo-entry "l" "* TODO %? %a\n" "Linked Project TODO"))   
      )

    (use-package calfw-org
      :config
      ;; 対象ファイル
      (setq cfw:org-icalendars nil)
      ;; First day of the week  0:Sunday, 1:Monday
      (setq calendar-week-start-day 1))
    
    )
  )

(eval-after-load 'ox ;; shouldn't be byte compiled.
  '(when (and user-init-file (buffer-file-name)) ;; don't do it in async
     (setq org-export-async-init-file "~/.emacs.d/init.el")
     )
  )


(defun my-add-current-timestamp()
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M"))
  )


(defun kiwon/org-agenda-redo-in-other-window ()
  "Call org-agenda-redo function even in the non-agenda buffer."
  (interactive)
  (let ((agenda-window (get-buffer-window org-agenda-buffer-name t)))
    (when agenda-window
      (with-selected-window agenda-window (org-agenda-redo)))))

(run-at-time nil 60 'kiwon/org-agenda-redo-in-other-window)

;;; 30-org.el ends here
