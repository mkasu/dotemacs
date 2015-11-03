;;; 10-look.el -- Looks and feels

;;; Commentary:
;; Looks and Style

;;; Code:

;; Splash Screen to Org-mode
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

;; Font Faces
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 180
                    :weight 'normal
                    :width 'normal)

;; Line Numbers
(global-linum-mode t)

;; No toolbar
(tool-bar-mode -1)

;; No bell
(setq visible-bell nil) ;; The default
(setq ring-bell-function 'ignore)

;; Custom themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; Theme
;;(load-theme 'wombat t)
(load-theme 'dracula)

(use-package powerline
  :ensure t
  :init (powerline-default-theme)
  )

(use-package spaceline
  :ensure t
  :init
  (progn
    (require 'spaceline-config)
    (setq powerline-default-separator 'wave)
    (spaceline-spacemacs-theme)))

;;; 10-look.el ends here
