;;; 31-latex.el --- LaTeX files

;;; Commentary:
;; Tools for working with LaTeX files

;;; Code:

(require 'use-package)

;; AucTeX
(use-package auctex
  :ensure t
  :mode ("\\.tex\\'" . latex-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :init
  (progn
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq-default TeX-master nil)
    (add-hook 'LaTeX-mode-hook 'visual-line-mode)
    (add-hook 'LaTeX-mode-hook 'flyspell-mode)
    (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
    (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
    (setq reftex-plug-into-AUCTeX t)
    (setq-default TeX-PDF-mode t)
    (setq-default TeX-engine 'xetex)

    ;; use Skim as default pdf viewer
    ;; Skim's displayline is used for forward search (from .tex to .pdf)
    ;; option -b highlights the current line; option -g opens Skim in the background  
    (setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
    (setq TeX-view-program-list
          '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g -r %n %o %b")))
    (setq-default TeX-auto-local "~/.auctex-auto")
    (add-hook 'TeX-mode-hook 'auto-fill-mode)
    (add-hook 'TeX-mode-hook
          '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))
    )
  )

;;; 31-latex.el ends here
