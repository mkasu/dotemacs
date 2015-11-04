;;; 42-prog-python.el --- Programming in Ruby

;;; Commentary:
;; Tools for working with Ruby

;;; Code:

(require 'use-package)

(use-package ruby-mode
  :ensure t
  :config
  (progn
    (use-package robe
      :ensure t
      :config
      (progn
        (add-to-list 'company-backends 'company-robe)
        (add-hook 'ruby-mode-hook 'robe-mode)
        )
      )
    )
  :mode (("\\.rake$" . ruby-mode)
         ("\\.gemspec$" . ruby-mode)
         ("\\.ru$" . ruby-mode)
         ("Rakefile$" . ruby-mode)
         ("Thorfile$" . ruby-mode)
         ("Gemfile$" . ruby-mode)
         ("Capfile$" . ruby-mode)
         ("Guardfile$" . ruby-mode))
  )

(use-package haml-mode
  :ensure t
  :mode "\\.haml\\'")

   
;;; 43-prog-ruby.el ends here
