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

    ;; clojure
    clojure-mode
    clojure-mode-extra-font-locking
    cider
    company

    ;; haskell
    haskell-mode

    ;; general
    golden-ratio
    magit
    helm))

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

;; minor modes for clojure-mode
(add-hook 'clojure-mode-hook #'enable-paredit-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'company-mode)

;; minor modes for cider/cider-repl
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)
(add-hook 'cider-repl-mode-hook #'paredit-mode)

;; company
;(setq company-idle-delay nil) ; never start completions automatically
;(global-set-key (kbd "M-TAB") #'company-complete) ; use meta+tab, aka C-M-i, as manual trigger

;; magit
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
