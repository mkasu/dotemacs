;;; 19-mail.el --- Mail

;;; Commentary:
;; Set up gnus

;;; Code:

(require 'use-package)

(use-package wanderlust
  :ensure t
  :defer t
  )

(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

(setq linum-mode-inhibit-modes-list '(eshell-mode
                                      shell-mode
                                      erc-mode
                                      jabber-roster-mode
                                      jabber-chat-mode
                                      gnus-group-mode
                                      gnus-summary-mode
                                      gnus-article-mode
                                      wl-summary-mode))

(defadvice linum-on (around linum-on-inhibit-for-modes)
  "Stop the load of linum-mode for some major modes."
    (unless (member major-mode linum-mode-inhibit-modes-list)
      ad-do-it))

(ad-activate 'linum-on)

;;(require 'org-wl)
(setq wl-init-file "~/.emacs.d/wl.el")

;;; 19-mail.el ends here 
