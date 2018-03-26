;;; Shell scripts

(add-to-list 'auto-mode-alist '("bashrc$" . sh-mode))
(add-to-list 'auto-mode-alist '("bash_profile$" . sh-mode))
(add-to-list 'auto-mode-alist '("bash_aliases$" . sh-mode))
(add-to-list 'auto-mode-alist '("bash_local$" . sh-mode))
(add-to-list 'auto-mode-alist '("bash_completion$" . sh-mode))
(add-to-list 'auto-mode-alist '(".powenv$" . sh-mode))

(add-hook 'shell-mode-hook (lambda () (setq tab-width 4)))

;;; Fix garbage characters in shell
(add-hook 'shell-mode-hook
     'ansi-color-for-comint-mode-on)

;; Stop shell from echoing all commands
(defun my-comint-init ()
  (setq comint-process-echoes t))

(add-hook 'comint-mode-hook 'my-comint-init)
