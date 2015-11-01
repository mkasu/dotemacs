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

;;; 42-prog-python.el ends here
