;;; 42-prog-python.el --- Programming in Python

;;; Commentary:
;; Tools for working with Python

;;; Code:

(use-package company-jedi
  :ensure t
  :config
  (progn
    (add-to-list 'company-backends 'company-jedi)
    )
  )

(require 'package)
(add-to-list 'package-archives
             '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(use-package elpy
  :commands elpy-enable
  :init (elpy-enable)
  :config
  (progn
    (setq elpy-rpc-backend "jedi")
    (setq elpy-rpc-python-command "python3")
    )
  )

(defun company-yasnippet-or-completion ()
  "Solve company yasnippet conflicts."
  (interactive)
  (let ((yas-fallback-behavior
         (apply 'company-complete-common nil)))
    (yas-expand)))

(add-hook 'company-mode-hook
          (lambda ()
            (substitute-key-definition
             'company-complete-common
             'company-yasnippet-or-completion
             company-active-map)))

;;; 42-prog-python.el ends here
