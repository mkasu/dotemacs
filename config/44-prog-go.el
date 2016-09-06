;;; 44-prog-go.el --- Programming in Go

;;; Commentary:
;; Tools for working with Go

;;; Code:

(add-hook 'after-init-hook 'global-company-mode)
(require 'company)
(require 'company-go)
(require 'go-mode-autoloads)

(add-to-list 'company-backends 'company-go)
(add-hook 'go-mode-hook 'company-mode)
(add-hook 'go-mode-hook (lambda ()
                            (Local-set-key (kbd \"M-.\") 'godef-jump)))

(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

;;; 42-prog-go.el ends here
