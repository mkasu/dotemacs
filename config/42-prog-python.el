;;; 42-prog-python.el --- Programming in Python

;;; Commentary:
;; Tools for working with Python

;;; Code:

(require 'package)
(add-to-list 'package-archives
             '("elpy" . "https://jorgenschaefer.github.io/packages/"))

;;(setenv "PYTHONHOME" "~/.pyenv/versions/anaconda3-4.1.1/lib/python3.5/")
;;(setenv "PYTHONPATH" "~/.pyenv/versions/anaconda3-4.1.1/lib/python3.5/site-packages/")

(use-package company-jedi
  :ensure t
  :config
  (progn
    (add-to-list 'company-backends 'company-jedi)
    )
  )

(use-package elpy
  :commands elpy-enable
  :init (elpy-enable)
  :config
  (progn
    (setq elpy-rpc-backend "jedi")
    (setq elpy-rpc-python-command "python")
    (setq pyenv-show-active-python-in-modeline t)
    (require 'pyenv-mode-auto)

    ;;(setq jedi:environment-root "env")
    ;;(setq jedi:environment-virtualenv
    ;;      (append python-environment-virtualenv
    ;;              '("--python" "python3")))
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

(add-hook 'python-mode-hook
          (lambda ()
		    (setq-default indent-tabs-mode t)
		    (setq-default tab-width 4)
		    (setq-default py-indent-tabs-mode t)
            (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

;;; 42-prog-python.el ends here
