;;; 41-prog-cpp.el --- Programming in C/C++

;;; Commentary:
;; Tools for working with C/C++

;;; Code:

(require 'use-package)

(setq c-default-style "stroustrup")
(setq c-basic-offset 4)

;; == Highlight organizational keywords ==
(add-hook 'c-mode-common-hook
               (lambda ()
                (font-lock-add-keywords nil
                                        '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

;; == Code folding ==
(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key (kbd "C-c <right>") 'hs-show-block)
    (local-set-key (kbd "C-c <left>")  'hs-hide-block)
    (local-set-key (kbd "C-c <up>")    'hs-hide-all)
    (local-set-key (kbd "C-c <down>")  'hs-show-all)
    (hs-minor-mode t)))

;; == Make .h an extension for cpp ==
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; == irony-mode ==
(use-package irony
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'objc-mode-hook 'irony-mode)
    (add-to-list 'irony-supported-major-modes 'glsl-mode)
    ;; replace the `completion-at-point' and `complete-symbol' bindings in
    ;; irony-mode's buffers by irony-mode's function
    (defun my-irony-mode-hook ()
      (define-key irony-mode-map [remap completion-at-point]
        'irony-completion-at-point-async)
      (define-key irony-mode-map [remap complete-symbol]
        'irony-completion-at-point-async))
    (add-hook 'irony-mode-hook 'my-irony-mode-hook)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
    (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
    )
  )

(use-package rtags
  :ensure t
  :demand
  :bind (("C-x C-<" . rtags-location-stack-back)
         ("C-x C-y" . rtags-find-symbol-at-point))
  )

(use-package cmake-ide
  :ensure t
  :init
  (cmake-ide-setup)
  )

(use-package cmake-mode
  :ensure t
  :mode (("/CMakeLists\\.txt\\'" . cmake-mode)
	 ("\\.cmake\\'" . cmake-mode)))

(use-package clang-format
  :ensure t
  :bind (("C-M-<tab>" . clang-format-buffer)
    )
  )

;;; 41-prog-cpp.el ends here
