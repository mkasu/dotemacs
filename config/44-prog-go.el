;;; 44-prog-go.el --- Programming in Go

;;; Commentary:
;; Tools for working with Go

;;; Code:


;; https://github.com/Schnouki/dotfiles/blob/master/emacs/init-30-modes.el
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :commands (godoc gofmt gofmt-before-save)
  :init
  (progn
    (require 'go-mode-autoloads)
    (setq gofmt-command "goimports")
    (folding-add-to-marks-list 'go-mode "// {{{" "// }}}" nil t)
    (defun schnouki/maybe-gofmt-before-save ()
      (when (eq major-mode 'go-mode)
	(gofmt-before-save)))
    (add-hook 'before-save-hook 'schnouki/maybe-gofmt-before-save)

    ;; From https://github.com/bradleywright/emacs.d
    ;; Update GOPATH if there's a _vendor (gom) or vendor (gb) dir
    (defun schnouki/set-local-go-path ()
      "Sets a local GOPATH if appropriate"
      (let ((current-go-path (getenv "GOPATH")))
        (catch 'found
          (dolist (vendor-dir '("_vendor" "vendor"))
            (let ((directory (locate-dominating-file (buffer-file-name) vendor-dir)))
              (when directory
                (make-local-variable 'process-environment)
                (let ((local-go-path (concat (expand-file-name directory) vendor-dir)))
                  (if (not current-go-path)
                      (setenv "GOPATH" local-go-path)
                    (unless (string-match-p local-go-path current-go-path)
                      (setenv "GOPATH" (concat local-go-path ":" current-go-path))))
                  (setq-local go-command
                              (concat "GOPATH=\"" local-go-path ":" (expand-file-name directory) ":${GOPATH}\" " go-command))
                  (throw 'found local-go-path))))))))
    (add-hook 'go-mode-hook 'schnouki/set-local-go-path))
  :config
  (progn
    ;; http://yousefourabi.com/blog/2014/05/emacs-for-go/
    (bind-key "C-c C-f" 'gofmt go-mode-map)
    (bind-key "C-c C-g" 'go-goto-imports go-mode-map)
    (bind-key "C-c C-k" 'godoc go-mode-map)
    (bind-key "C-c C-r" 'go-remove-unused-imports go-mode-map)))

(use-package company-go
  :ensure t
  :commands company-go
  :init (add-to-list 'company-backends 'company-go)
  :config
  (progn
    (add-to-list 'company-backends 'company-go)
    (add-hook 'go-mode-hook 'company-mode)
    ))

(use-package go-eldoc
  :ensure t
  :commands go-eldoc-setup
  :init (add-hook 'go-mode-hook 'go-eldoc-setup))



;;; 42-prog-go.el ends here
