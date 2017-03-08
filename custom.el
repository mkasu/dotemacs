(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -synctex=1")
 '(TeX-source-correlate-mode t)
 '(TeX-view-program-list
   (quote
	(("Okular"
	  ("okular --unique \\\"file:\\\"%s.pdf\\\"#src:%n %a\\\"")
	  ""))))
 '(company-quickhelp-color-background "#4e4e4e")
 '(company-quickhelp-color-foreground "#5fafd7")
 '(company-quickhelp-delay 0.1)
 '(company-quickhelp-mode t)
 '(company-quickhelp-use-propertized-text t)
 '(elpy-modules
   (quote
	(elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-yasnippet elpy-module-sane-defaults)))
 '(minimap-hide-fringes t)
 '(minimap-mode t)
 '(minimap-window-location (quote right))
 '(org-agenda-files
   (quote
	("/home1/kastnerm/Seafile/org/emacs.org" "/home1/kastnerm/Seafile/org/gm.org" "/home1/kastnerm/Seafile/org/inbox.org" "/home1/kastnerm/Seafile/org/phd-diary.org" "/home1/kastnerm/Seafile/org/test.org")))
 '(package-selected-packages
   (quote
	(langtool minimap company-quickhelp json-mode pkgbuild-mode git-gutter mozc htmlize company go-mode helm helm-core jedi-core markdown-mode projectile python-environment pythonic swiper pcache org helm-projectile use-package org-ehtml org-preview-html framemove deft company-statistics hl-todo highlight-indentation highlight-chars xpm wanderlust undo-tree swiper-helm spaceline rtags robe pyenv-mode-auto popwin paradox org-projectile moe-theme migemo magit jade-mode helm-ag haml-mode go-eldoc glsl-mode flycheck elpy dracula-theme desktop+ darkokai-theme d-mode company-jedi company-irony company-go color-theme-sanityinc-tomorrow cmake-mode cmake-ide clang-format auctex)))
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#303030" :foreground "#c6c6c6" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :family "Source Code Pro"))))
 '(company-scrollbar-bg ((t (:background "#4d5367"))))
 '(company-scrollbar-fg ((t (:background "#424758"))))
 '(company-tooltip ((t (:inherit default :background "#4d5367"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 '(minimap-active-region-background ((t (:background "saddle brown"))))
 '(minimap-font-face ((t (:height 20 :family "DejaVu Sans Mono")))))
