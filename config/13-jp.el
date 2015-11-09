
;;; 13-jp.el --- Japanese

;;; Commentary:
;; Set up japanese support

;;; Code:

;; for emacs 24.3
;; http://wp.kncn.net/archives/6025
;; (setq search-whitespace-regexp nil)

(require 'use-package)

;; ローマ字で日本語の検索
;; (use-package migemo
;;   :ensure t
;;   :defer 20
;;   :config
;;   (migemo-init)
;;   (setq migemo-options '("-q" "--emacs"))
;;   (setq migemo-user-dictionary nil)
;;   (setq migemo-regex-dictionary nil)
;;   (setq migemo-coding-system 'utf-8-unix)

;;   (setq migemo-command "cmigemo")
;;   (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict"))

;; 日本語入力
(defconst my-lisp-dir (cond
    ((equal system-type 'gnu/linux) "/usr/share/emacs/site-lisp/")
    ((equal system-type 'darwin) (concat "/usr/local/Cellar/emacs/" (number-to-string emacs-major-version) "." (number-to-string emacs-minor-version) "/share/emacs/site-lisp/"))
    (t (concat "/usr/local/emacs/site-lisp/"))))

(add-to-list 'load-path (concat my-lisp-dir "skk"))

(use-package skk-autoloads
 :bind (("C-x j" . skk-mode))
 :init
 ;; 補完時にサイクルする 
 (setq skk-comp-use-prefix t)
 (setq skk-comp-circulate t)
 ;; 個人辞書の文字コードを指定 
 ;;(setq skk-jisyo-code 'utf-8)

 ;; migemo を利用するため isearch 中は無効
 (setq skk-isearch-mode-enable nil)
 (setq skk-isearch-start-mode 'latin)
 
 (setq skk-large-jisyo "~/.emacs.d/dict/SKK-JISYO.L")

 (setq skk-egg-like-newline t)		  ; Enterで改行しない
 (setq skk-use-look t)					   ; 英語補完
 (setq skk-byte-compile-init-file t) ;; 自動バイトコンパイル

 (setq skk-show-candidates-always-pop-to-buffer t) ; 変換候補の表示位置
 (setq skk-henkan-show-candidates-rows 2) ; 候補表示件数を2列に
)

;;; 13-jp.el ends here 
