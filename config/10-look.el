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
                    ;;:family "Source Han Code JP"
                    :family "Ricty Diminished"
                    :height 210
                    :weight 'normal
                    :width 'normal)

;; Line Numbers
(global-linum-mode t)

;; No toolbar
(tool-bar-mode -1)

;; No bell
(setq visible-bell nil) ;; The default
(setq ring-bell-function 'ignore)

;; Show matching parentheses
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Custom themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; Theme
;;(load-theme 'wombat t)
(load-theme 'dracula t)

(require 'color)
  
(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

(require 'use-package)

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
