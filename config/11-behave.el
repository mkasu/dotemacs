;;; 11-behave.el --- Behaviour

;;; Commentary:
;; Change behaviour of emacs

;;; Code:

;; Start server
(server-start)

;; OS X specific
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq ns-right-alternate-modifier nil)

;; Easy switching between windows
(windmove-default-keybindings 'super)

;; Electric Pair mode
(electric-pair-mode 1)

;; Delete Selection mode
(delete-selection-mode 1)

;; Highlight line
(global-hl-line-mode 1)

;; Session management
(desktop-save-mode 1)

;; Backup
(setq make-backup-files nil)
(setq backup-directory-alist
     `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
     `((".*" ,temporary-file-directory t)))

;; 4 spaces for tab
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; Special window in bottom
;; (global-set-key (kbd "C-c q") 'my-quit-bottom-side-windows)

;; (add-to-list 'display-buffer-alist
;;              `(,(rx bos "*" (* not-newline) "*" eos)
;;                (display-buffer-in-side-window)
;;                (reusable-frames . visible)
;;                (inhibit-same-window . t)
;;                (side            . bottom)
;;                (window-height   . 0.25)))

;; (defun my-quit-bottom-side-windows ()
;;     "Quit side windows of the current frame."
;;       (interactive)
;;         (dolist (window (window-at-side-list))
;;               (quit-window nil window)))


(use-package popwin
  :ensure t
  :config
  (progn
    (popwin-mode 1)
    (push '(flycheck-error-list-mode :stick t) popwin:special-display-config)
    (push '("^\*helm.+\*$" :regexp t) popwin:special-display-config)
    (add-hook 'helm-after-initialize-hook (lambda ()
                                          (popwin:display-buffer helm-buffer t)
                                          (popwin-mode -1)))
    (add-hook 'helm-cleanup-hook (lambda () (popwin-mode 1)))
    )
  )

;;; === Packages ===

(use-package helm
  :ensure t
  :config (helm-mode 1)
  )

;;; 11-behave.el ends here
