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

    ;; Use Skim as viewer, enable source <-> PDF sync
    ;; make latexmk available via C-c C-c
    ;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
    ;; (add-hook 'LaTeX-mode-hook (lambda ()
    ;;                              (push
    ;;                               '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
    ;;                                 :help "Run latexmk on file")
    ;;                               TeX-command-list)))
    ;; (add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

    ;; use Skim as default pdf viewer
    ;; Skim's displayline is used for forward search (from .tex to .pdf)
    ;; option -b highlights the current line; option -g opens Skim in the background  
    (setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
    (setq TeX-view-program-list
          '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g -r %n %o %b")))
    )
  )

;;; 31-latex.el ends here
