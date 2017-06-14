;; Define package repositories
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; Install manually with M-x package-install
;; list of packages to install:
(defvar active-packages
  '(
    ;; lisp
    paredit
    rainbow-delimiters

    ;; racket
    ;;racket-mode

    ;; scheme + racket
    geiser

    ;; clojure
    clojure-mode
    clojure-mode-extra-font-locking
    cider
    company
    clj-refactor

    ;; haskell
    haskell-mode

    ;; general
    golden-ratio
    magit
    helm
    ido-vertical-mode
    restclient
    ace-window
    json-mode

    ;;golang
    go-mode

    ;;elm lang
    elm-mode

    ;; rust lang
    rust-mode

    erlang))

;; Fix $PATH in OS X
(if (eq system-type 'darwin)
    (add-to-list 'active-packages 'exec-path-from-shell))

;; install packages
(dolist (p active-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; load packages
(add-to-list 'load-path "~/.emacs.d/elpa")
(dolist (p active-packages)
  (when (package-installed-p p)
    (load (symbol-name p))))

;; initialise exec-path-from-shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; CLOJURE_MODE
(setq clojure-indent-style :align-arguments) ;;:always-indent)

;; minor modes for cider/cider-repl
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)
(add-hook 'cider-repl-mode-hook #'paredit-mode)
(setq cider-repl-use-pretty-printing t)
(setq cider-repl-print-length 50)
;; or do this is the repl (set! *print-length* 50)
(setq cider-test-show-report-on-success t)

(defun my-cider-mode-hook ()
  ;; disable cider-refresh
  ;; call cider-refresh manually using M-x - when needed
  (define-key cider-mode-map (kbd "C-c C-x") nil))

(add-hook 'cider-repl-mode-hook 'my-cider-mode-hook)

;; minor modes for clojure-mode
(add-hook 'clojure-mode-hook #'enable-paredit-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'company-mode)

;; Company
;;(setq company-idle-delay nil) ; never start completions automatically
(setq company-idle-delay 0.3)
;;(global-set-key (kbd "M-TAB") #'company-complete) ; use meta+tab, aka C-M-i, as manual trigger

;; Magit
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; helm
(require 'helm)
(require 'helm-config)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
;(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
;; disable helm completion
(setq helm-mode-handle-completion-in-region nil)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
;; use ido for find file
(add-to-list 'helm-completing-read-handlers-alist '(find-file . ido))

(setq helm-quick-update                     t ; do not display invisible candidates
      helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-adaptive-history-file            "~/.emacs.d/data/helm-history")

(require 'ido-vertical-mode)
(ido-vertical-mode t)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;; Golden ratio
(require 'golden-ratio)
(golden-ratio-mode 1)

;; Magit
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; Haskell mode
(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; clj-refactor
(require 'clj-refactor)
(defun clj-refactor-clojure-mode ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c C-m"))
(add-hook 'clojure-mode-hook #'clj-refactor-clojure-mode)

;; golden ratio trigger on ace-window
(require 'ace-window)
(global-set-key (kbd "C-x o") 'ace-window)
;; (global-set-key (kbd "M-p") 'ace-window) ;; cider REPL mode overwrites M-p and M-n
(add-to-list 'golden-ratio-extra-commands 'ace-window)
(add-to-list 'golden-ratio-extra-commands 'magit-status)

;; find-file-at-point, useful with grep and find results
;;(require 'ffap)
;;(global-set-key (kbd "C-c p") 'find-file-at-point)

;; GEISER - for scheme and racket
(require 'geiser)
(setq geiser-mit-binary "scheme")
(setq geiser-active-implementations '(mit racket))
;; enable other minor modes for geiser repl
(add-hook 'geiser-repl-mode-hook #'enable-paredit-mode)
(add-hook 'geiser-repl-mode-hook #'rainbow-delimiters-mode)
;; enabble other minor modes for geiser (buffer)
(add-hook 'geiser-mode-hook #'enable-paredit-mode)
(add-hook 'geiser-mode-hook #'rainbow-delimiters-mode)

;; ==== GENERAL ====

(delete-selection-mode 1)
;; making paredit work with delete-selection-mode
(put 'paredit-forward-delete 'delete-selection 'supersede)
(put 'paredit-backward-delete 'delete-selection 'supersede)

;; erlang initializations
(require 'erlang-start)
