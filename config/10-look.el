;;; 10-look.el -- Looks and feels

;;; Commentary:
;; Looks and Style

;;; Code:



;; Font Faces
(if (display-graphic-p)
    (progn
      ;; Splash Screen to Org-mode
      (setq inhibit-splash-screen t
            initial-scratch-message nil
            initial-major-mode 'org-mode)
      
      (set-face-attribute 'default nil
                          ;;:family "Source Code Pro" ;; no Japanese glyphs
                          ;;:family "Source Han Code JP" ;; I dislike the proportion of Japanese glyphs to rest
                          ;;:family "M+ 1MN" ;; Too thin
                          ;; :family "Ricty" ;; Mix Inconsolata and M+ Japanese glpyhs
                          :family "Source Code Pro"
                          :height 110 ;; Size is mainly due to Retina display
                          :weight 'normal
                          :width 'normal)

      ;; (set-fontset-font (frame-parameter nil 'font)
      ;;                   'japanese-jisx0208
      ;;                   (cons "Ricty Diminished" "iso10646-1"))
      ;; (set-fontset-font (frame-parameter nil 'font)
      ;;                   'japanese-jisx0212
      ;;                   (cons "Ricty Diminished" "iso10646-1"))
      ;; (set-fontset-font (frame-parameter nil 'font)
      ;;                   'katakana-jisx0201
      ;;                   (cons "Ricty Diminished" "iso10646-1"))

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
      )

  )

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
