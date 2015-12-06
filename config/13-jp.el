
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
                       (t (concat "/usr/local/emacs/site-lisp/"))))

(add-to-list 'load-path (concat my-lisp-dir "skk"))

(global-unset-key "\C-x\C-j")
(use-package skk-autoloads
  :bind (("C-x j" . skk-mode)
         ("C-x C-j" . skk-mode))
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
  (setq skk-delete-implies-kakutei nil) ; ▼モードで一つ前の候補を表示
  (setq skk-show-annotation t)		  ; Annotation
  (setq skk-henkan-strict-okuri-precedence t) ; 送り仮名が厳密に正しい候補を優先して表示
  )


(use-package skk-server
  :config
  (require 'skk-vars)
  ;; 辞書サーバを利用する場合の設定
  (setq ;; skk-server-host "0.0.0.0"
   skk-server-host "localhost" ;; windows だとこっち
   skk-server-prog "google-ime-skk" ;; パスは通っているようだ. 
   skk-server-portnum 1178)

  ;; 辞書サーバが使用不能になると辞書ファイルを 
  ;; Emacs のバッファに読み込んで 検索を行う.
  (setq skk-server-inhibit-startup-server nil) ;; 通信エラー時はローカル辞書を.
  (setq skk-server-jisyo "~/.emacs.d/dict/SKK-JISYO.L")

  (eval-after-load "skk"
    '(progn
       (add-to-list 'skk-search-prog-list
                    '(skk-server-completion-search) t)
       (add-to-list 'skk-search-prog-list
                    '(skk-comp-by-server-completion) t)))

  ;; 辞書登録の際に送り仮名を削除
  (setq skk-check-okurigana-on-touroku 'auto)
  ;;漢字登録のミスをチェックする
  (setq skk-check-okurigana-on-touroku t))

;;; 13-jp.el ends here 
