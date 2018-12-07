;;; Customise specific modes in this file ;;;

;;; CUA mode
(cua-mode t)

;;; HTML mode

;; Set default indent and tab for html modes
(add-hook 'html-mode-hook
   (lambda ()
     ;; Default indentation is usually 2 spaces, changing to 4.
     (set (make-local-variable 'sgml-basic-offset) 2)))

;; SGML offset
(setq sgml-basic-offset 2)

;; Set standard indent size
(setq standard-indent 2)

;; Switch indent to spaces (not sure the difference)
(setq-default indent-tabs-mode nil)

;; Switch indent to spaces
(setq indent-tabs-mode nil)

;; Set standard tab size
(setq default-tab-width 4)

;; Set basic indentation in c-mode (and it's derivatives)
(setq c-basic-offset 4)

(setq tab-width 4)

;;; Shell scripts ;;;

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

;;; Dired functions
(put 'dired-find-alternate-file 'disabled nil)
(put 'erase-buffer 'disabled nil)

;; save open buffers before exiting
(desktop-save-mode 1)

;;; Turn on automatic encription for GPG files
(require 'epa-file)
(epa-file-enable)

;; Interactively Do Things
;; (require 'ido)
;; (ido-mode t)

;;; === CSS === ;;
(setq css-indent-offset 2)
(add-hook 'css-mode #'company-mode)


;;; === Delete section === ;;;
(delete-selection-mode 1)
;; making paredit work with delete-selection-mode
(put 'paredit-forward-delete 'delete-selection 'supersede)
(put 'paredit-backward-delete 'delete-selection 'supersede)

;;; === window layout undo/redo === ;;;
(setq winner-mode t)
