;;; 40-prog-gen.el --- General programming

;;; Commentary:
;; Tools related to programming languages

;;; Code:

;; == Recompile Hotkey ==
(global-set-key (kbd "C-c c") 'compile-again)
(setq compilation-last-buffer nil)
(defun compile-again (pfx)
  """Run the same compile as the last time.

If there was no last time, or there is a prefix argument, this acts like
M-x compile.
"""
 (interactive "p")
 (if (and (eq pfx 1)
      compilation-last-buffer)
     (progn
       (set-buffer compilation-last-buffer)
       (revert-buffer t t))
   (call-interactively 'compile)))

;; == highlight TODO/FIXME etc. ==
(setq hl-todo-keyword-faces
  '(("HOLD" . "#d0bf8f")
    ("TODO" . "#cc9393")
    ("NEXT" . "#dca3a3")
    ("THEM" . "#dc8cc3")
    ("PROG" . "#7cb8bb")
    ("OKAY" . "#7cb8bb")
    ("DONT" . "#5f7f5f")
    ("FAIL" . "#8c5353")
    ("DONE" . "#afd8af")
    ("FIXME" . "#cc9393")
    ("XXX"   . "#cc9393")
    ("XXXX"  . "#cc9393")
    ("???"   . "#cc9393")))
(global-hl-todo-mode 1)

;; == yasnippet ==
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :init
  (progn
    (yas-global-mode 1)))

;; == company-mode ==
(use-package company
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (use-package company-irony :ensure t :defer t)
  (setq company-idle-delay          0
	company-minimum-prefix-length   2
	company-show-numbers            t
	company-tooltip-limit           20
	company-dabbrev-downcase        nil
	company-backends                '((company-irony))
	)
  :bind ("C-;" . company-complete-common)
  )

;; == projectile ==
(use-package projectile
  :ensure t
  :init
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (use-package helm-projectile
    :ensure t
    :bind (("C-x C-g" . helm-mini)
           ("C-x C-f" . helm-find-files))
    :init
    (helm-projectile-on)
    )
  )

;; == swiper ==

(use-package swiper-helm
  :ensure t
  :bind ("C-c C-g" . swiper)
  )

;; == magit ==
(use-package magit
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status)
  :config(
          (magit-diff-use-overlays nil)
          )
  )

;; == flycheck ==
(use-package flycheck
  :ensure t
  :defer t
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (progn
    (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))))

;; == ag ==

(use-package helm-ag
  :ensure t
  :bind ("C-c C-f" . helm-do-ag-project-root)
  )

;; == glsl ==
(use-package glsl-mode
  :ensure t
  :if (eq system-type 'darwin)
  :init
  (append auto-mode-alist '('("\\.glsl\\'" . glsl-mode)
                            '("\\.vert\\'" . glsl-mode)
                            '("\\.frag\\'" . glsl-mode)
                            '("\\.geom\\'" . glsl-mode))))

;; == swift mode ==
;(use-package swift-mode
;  :ensure t
;  :config
;  (add-to-list 'flycheck-checkers 'swift)
;  (defvar flycheck-swift-sdk-path)
;  (setq flycheck-swift-sdk-path "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.11.sdk")
;  )

;;; 40-prog-gen.el ends here
