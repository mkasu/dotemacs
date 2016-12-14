
;;; 13-jp.el --- Japanese

;;; Commentary:
;; Set up japanese support

;;; Code:

;; for emacs 24.3
;; http://wp.kncn.net/archives/6025
;; (setq search-whitespace-regexp nil)

(require 'use-package)

;; 日本のカレンダー
;;(use-package japanese-holidays
;;  :ensure t
;;  :config
;;  (setq calendar-holidays
;;        (append japanese-holidays local-holidays other-holidays))
;;  (setq mark-holidays-in-calendar t))

;; ローマ字で日本語の検索
(use-package migemo
  :ensure t
  :demand t
  :config
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)

  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  (migemo-init))

;; 日本語入力
(defconst my-lisp-dir (cond
                       ((equal system-type 'gnu/linux) "/usr/share/emacs/site-lisp/")
                       ((equal system-type 'darwin) (concat "/usr/local/Cellar/emacs/" (number-to-string emacs-major-version) "." (number-to-string emacs-minor-version) "/share/emacs/site-lisp/"))
                       (t (concat "/usr/share/emacs24/site-lisp/"))))

(add-to-list 'load-path (concat my-lisp-dir "emacs-mozc"))

(use-package mozc
  :config
  (require 'mozc)

  (set-language-environment "Japanese")
  (setq default-input-method "japanese-mozc")

  (global-set-key (kbd "<zenkaku-hankaku>") 'toggle-input-method)

  (add-hook 'mozc-mode-hook
   (lambda()
     (define-key mozc-mode-map (kbd "<zenkaku-hankaku>") 'toggle-input-method)))
  )



;;; 13-jp.el ends here
