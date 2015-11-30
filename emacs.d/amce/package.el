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
  '(paredit
    clojure-mode
    clojure-mode-extra-font-locking
    clj-refactor
    cider
    projectile
    rainbow-delimiters
    tagedit
    company

    magit
    guide-key
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
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(helm-mode 1)
(helm-autoresize-mode 1)
