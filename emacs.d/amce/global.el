;;; Generic emacs settings I cannot live without

;; Use command as the meta key; option key as super
;;(setq ns-command-modifier 'meta)
;;(setq ns-option-modifier  'super)

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

;; Modeline info
(display-time-mode 1)
;; (display-battery-mode 1)

;; Small fringes
(set-fringe-mode '(1 . 1))

;; Emacs gurus don't need no stinking scroll bars
(when (fboundp 'toggle-scroll-bar)
  (toggle-scroll-bar -1))

;; Explicitly show the end of a buffer
(set-default 'indicate-empty-lines t)

;; Line-wrapping
(set-default 'fill-column 78)

;; Prevent the annoying beep on errors
;; (setq visible-bell t)

;; Make sure all backup files only live in one place
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms
          `((".*" ,"~/.emacs.d/auto-save")))

;; Gotta see matching parens
(show-paren-mode t)

;; Don't truncate lines
(setq truncate-lines t)
(setq truncate-partial-width-windows nil)

;; For emacsclient
;;(server-start)

;; Trailing whitespace is unnecessary
(defvar whitespace-cleanup-on-save t)
;; (setq whitespace-cleanup-on-save nil)
(add-hook 'before-save-hook
      (lambda ()
        (if whitespace-cleanup-on-save (whitespace-cleanup))))

;; Trash can support
(setq delete-by-moving-to-trash t)

;; `brew install aspell --lang=en` (instead of ispell)
;;(setq-default ispell-program-name "aspell")
;;(setq ispell-list-command "list")
;;(setq ispell-extra-args '("--sug-mode=ultra"))

;; zap-up-to-char, forward-to-word, backward-to-word, etc
;;(require 'misc)

;; Hide the annoying toolbar (which in Ubuntu 10 cause focus to freeze emacs)
(tool-bar-mode -1)

;; Highlight the cursor's current line
(global-hl-line-mode 0)

;; SGML offset
(setq sgml-basic-offset 4)

;; Set standard indent size
(setq standard-indent 4)

;; Switch indent to spaces (not sure the difference)
(setq-default indent-tabs-mode nil)

;; Switch indent to spaces
(setq indent-tabs-mode nil)

;; Set standard tab size
(setq default-tab-width 4)

;; Set basic indentation in c-mode (and it's derivatives), and th default tab width
(setq c-basic-offset 4)
(setq tab-width 4)

;; Set emacs to scroll text 1 line at a time
(setq scroll-step 1)

(require 'epa-file)
(epa-file-enable)

;; Buffers use filename as title
(setq frame-title-format '("%b"))

;; Copy to cliboard
(setq x-select-enable-clipboard t)
(put 'upcase-region 'disabled nil)

;; Show full path in buffer name
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
    '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))
