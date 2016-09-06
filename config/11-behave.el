;;; 11-behave.el --- Behaviour

;;; Commentary:
;; Change behaviour of emacs

;;; Code:

;; Start server
(if (display-graphic-p)
    (progn
      (server-start)
      )
    )

;; OS X specific
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/Library/TeX/texbin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/Library/TeX/texbin")))

(setq ns-right-alternate-modifier nil)

;; Short yes-or-no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Remove menu bar for maximum screen space
(menu-bar-mode -1)

;; Easy switching between windows
(windmove-default-keybindings 'super)

;; Electric Pair mode
(electric-pair-mode 1)

;; Delete Selection mode
(delete-selection-mode 1)

;; Highlight line
(global-hl-line-mode 1)

;; Session management
;(desktop-save-mode 1)

;; Backup behaviour
(auto-save-mode 1)

(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
        (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))

(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 9
  kept-old-versions 6
  version-control t)

;; Read from hard-disk
;; Especially useful when syncing between different computers (Dropbox)
;; Or using other editors than emacs simultaneously (why would anyone not use Emacs)
(global-auto-revert-mode 1)

;; 4 spaces for tab
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; Terminal
;; (defun popwin-term:ansi-term () 
;;   (interactive)
;;   (popwin:display-buffer-1
;;   (or (get-buffer "*ansi-term*")
;;       (save-window-excursion
;;       (interactive)
;;       (ansi-term "/usr/local/bin/zsh")))
;;   :default-config-keywords '(:position :bottom :height 20 :stick t)))
;; (global-set-key (kbd "C-x t") 'popwin-term:ansi-term)



;; Special window in bottom
(require 'use-package)

(use-package popwin
  :ensure t
  :config
  (progn
    (popwin-mode 1)
    (push '(flycheck-error-list-mode :stick t) popwin:special-display-config)
    (push '("^\*helm.+\*$" :regexp t) popwin:special-display-config)
    (push '("\\*ansi-term.*\\*" :regexp t) popwin:special-display-config)
    (add-hook 'helm-after-initialize-hook (lambda ()
                                          (popwin:display-buffer helm-buffer t)
                                          (popwin-mode -1)))
    (add-hook 'helm-cleanup-hook (lambda () (popwin-mode 1)))
    (push '("*eshell*" :height 0.5) popwin:special-display-config)
    )
  )

(defun eshell-pop (universal-argument)
  "open eshell window using popwin-elf"
  (interactive "P")
  (let* ((eshell-buffer-name "*eshell*")
         (eshell-buffer (get-buffer eshell-buffer-name))
         (file-name (buffer-file-name (current-buffer)))
         (current-directory (with-current-buffer (current-buffer) default-directory)))
    (if eshell-buffer
        (popwin:display-buffer eshell-buffer)
      (eshell))
    (when (and universal-argument file-name)
      (eshell-kill-input)
      (insert (concat "cd " current-directory))
      (eshell-send-input)
      (end-of-buffer))))
(global-set-key (kbd "C-c t") 'eshell-pop)
    
;;; === Packages ===

(use-package helm
          :diminish helm-mode
          :ensure t
          :init
          (progn
            (require 'helm-config)
            (helm-mode)
            )
          :bind (
            ("M-x" . helm-M-x)
            )
          )

(use-package undo-tree
  :diminish undo-tree-mode
  :init (global-undo-tree-mode)
  :ensure t
  :config
  (progn
    (global-set-key (kbd "C-S-z") 'undo-tree-redo)
    (global-set-key (kbd "C-z") 'undo)
    )
  )

;; (use-package which-key
;;   :config (which-key-mode)
;;   )


(use-package desktop+
  :ensure t)

;;; 11-behave.el ends here
