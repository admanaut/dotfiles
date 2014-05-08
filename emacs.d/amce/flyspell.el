;;; Flyspell settings

;; Enable it for text-mode-hook org-mode-hook
(add-hook 'text-mode-hook 'flyspell-mode)
;;(add-hook 'org-mode-hook 'flyspell-mode)

;; Disable printing messages for every word
(setq flyspell-issue-message-flag nil)
