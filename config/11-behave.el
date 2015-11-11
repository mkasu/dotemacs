;;; 11-behave.el --- Behaviour

;;; Commentary:
;; Change behaviour of emacs

;;; Code:

;; Start server
(server-start)

;; OS X specific
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/Library/TeX/texbin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/Library/TeX/texbin")))

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
(auto-save-mode 1)

;; Read from hard-disk
;; Especially useful when syncing between different computers (Dropbox)
;; Or using other editors than emacs simultaneously (why would anyone not use Emacs)
(global-auto-revert-mode 1)

;; 4 spaces for tab
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; Special window in bottom
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

(use-package which-key
  :config (which-key-mode)
  )


(use-package desktop+
  :ensure t)

;;; 11-behave.el ends here
