;;; Generic emacs settings

;; Don't show the startup screen
(setq inhibit-startup-message t)

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Highlight regions and add special behaviors to regions.
;; "C-h d transient" for more info
(setq transient-mark-mode t)
(pending-delete-mode t)

;; Display line and column numbers
(setq line-number-mode    t)
(setq column-number-mode  t)

;; time and battery info
(display-time-mode 1)
(display-battery-mode 1)

;; Small fringes
(set-fringe-mode '(1 . 1))

;; No scroll bars
(when (fboundp 'toggle-scroll-bar)
  (toggle-scroll-bar -1))

;; Hide toolbar
(tool-bar-mode -1)

;; Explicitly show the end of a buffer
(set-default 'indicate-empty-lines t)

;; Line-wrapping
(set-default 'fill-column 80)

;; Prevent beep on errors
(setq visible-bell t)

;;; === Backups === ;;;
(setq delete-old-versions 1)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms `((".*" ,"~/.emacs.d/auto-save")))

;;; === History === ;;;
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
	search-ring
	regexp-search-ring))


;; show matching parens
(show-paren-mode t)

;; Don't truncate lines
(setq truncate-lines t)
(setq truncate-partial-width-windows nil)

;; Trailing whitespace is unnecessary
(defvar whitespace-cleanup-on-save t)
;; (setq whitespace-cleanup-on-save nil)
(add-hook 'before-save-hook
  (lambda ()
    (if whitespace-cleanup-on-save (whitespace-cleanup))))

;; Trash can support
(setq delete-by-moving-to-trash t)

;; Highlight the cursor's current line
(global-hl-line-mode 0)

;; Set emacs to scroll text 1 line at a time
(setq scroll-step 1)

;; Buffers use filename as title
(setq frame-title-format '("%b"))

;; Copy to cliboard
(setq x-select-enable-clipboard t)
(put 'upcase-region 'disabled nil)

;; Show full path in buffer name
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
    '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;;; Theme customization
(load-theme 'wombat)

;;; Fonts
(when (eq system-type 'darwin)
  ;; default Latin font (e.g. Consolas)
  ;; default font size (point * 10)
  (set-face-attribute 'default nil :family "Monaco" :height 120 :weight 'normal))

;;; Gotta do UTF-8
(require 'un-define "un-define" t)
(set-buffer-file-coding-system 'utf-8 'utf-8-unix)
(set-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
;; (set-default default-buffer-file-coding-system 'utf-8-unix)

;;; Upcase and downcase regions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; Sets the current horizontal position for C-n and C-p
(put 'set-goal-column 'disabled nil)

;;; Restrict buffer editing to a region
(put 'narrow-to-region 'disabled nil)

;; Use command as the meta key; option key as super
;;(setq ns-command-modifier 'meta)
;;(setq ns-option-modifier  'super)

;; `brew install aspell --lang=en` (instead of ispell)
;;(setq-default ispell-program-name "aspell")
;;(setq ispell-list-command "list")
;;(setq ispell-extra-args '("--sug-mode=ultra"))


;;; === Recent files === ;;;
(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(global-set-key (kbd "C-c f r") 'recentf-open-files)
(recentf-mode)
