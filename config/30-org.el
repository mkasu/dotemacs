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
    (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
      (add-hook hook (lambda () (flycheck-mode -1))))
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
                          (org-agenda-use-time-grid t))
                      )       
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
    (setq org-track-ordered-property-with-tag t)
    (setq org-enforce-todo-dependencies t)
    (setq org-agenda-dim-blocked-tasks 'invisible)
    (setq org-agenda-show-inherited-tags 'nil)
    (setq org-support-shift-select t)
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
    (setq org-agenda-time-grid   '((daily today)
                                   "--------------------"
                                   (800 1000 1200 1400 1600 1800 2000 2200)))
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
            ("e" "Email Task" entry
             (file+headline "~/Dropbox/org/todo.org" "Email")
             "* TODO %^{Title}
%a
%?
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

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (gnuplot . t)
     (latex . t)
     (ledger . t) 
     (ocaml . nil)
     (python . t)
     (ruby . t)
     (screen . nil)
     (sh . t)
     (sql . nil)
     (sqlite . t)))
  
  (setq org-confirm-babel-evaluate nil)

  (defun org-babel-remove-result-buffer ()
  "Remove results from every code block in buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-babel-src-block-regexp nil t)
      (org-babel-remove-result))))
  (global-set-key (kbd "C-c C-v C-k") 'org-babel-remove-result-buffer)

  (use-package org-depend
    )

  (use-package org-mac-link)
  
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

;; I don't want to be warned about discarding undo info.
(unless (boundp 'warning-suppress-types)
  (setq warning-suppress-types nil))
(push '(undo discard-info) warning-suppress-types)

;; Provides function to export current org buffer as JSON structure
;; to $file.org.json. Adapted from an org-mode mailing post by
;; Brett Viren: https://lists.gnu.org/archive/html/emacs-orgmode/2014-01/msg00338.html
(require 'json)
(defun org-export-json ()
  (interactive)
  (let* ((tree (org-element-parse-buffer 'object nil)))
    (org-element-map tree (append org-element-all-elements
                                  org-element-all-objects '(plain-text))
      (lambda (x)
        (if (org-element-property :parent x)
            (org-element-put-property x :parent "none"))
        (if (org-element-property :structure x)
            (org-element-put-property x :structure "none"))
        ))
    (write-region
     (json-encode tree)
     nil (concat (buffer-file-name) ".json"))))

(defun cli-org-export-json ()
  (let ((org-file-path (car command-line-args-left))
        (other-load-files (cdr command-line-args-left)))
    (mapc 'load-file other-load-files)
    (find-file org-file-path)
    (org-mode)
    (message "Exporting to JSON: %s" (car command-line-args-left))
    (org-export-json)))

;;; 30-org.el ends here
