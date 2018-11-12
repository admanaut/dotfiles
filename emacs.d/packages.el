(require 'package)

(add-to-list
  'package-archives
  '("melpa" . "http://melpa.org/packages/")
  t)
(package-initialize)
(package-refresh-contents)


;;; === use package === ;;;
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(require 'use-package)

;;; automatically recompile Emacs Lisp source files ;;;
(use-package auto-compile
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)

;;; Libraries
(use-package dash)

;;; exec-path-from-shell ;;;
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


;;; === IDO vertical === ;;;
(use-package ido-vertical-mode)
(ido-vertical-mode t)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;;; === CLOJURE MODE === ;;;
(setq clojure-indent-style :align-arguments) ;;:always-indent)
;; ;; minor modes for cider/cider-repl
;; (add-hook 'cider-repl-mode-hook #'company-mode)
;; (add-hook 'cider-mode-hook #'company-mode)
;; (add-hook 'cider-repl-mode-hook #'paredit-mode)
;; (setq cider-repl-use-pretty-printing t)
;; (setq cider-repl-print-length 50)
;; ;; or do this is the repl (set! *print-length* 50)
;; (setq cider-test-show-report-on-success t)
;; (defun my-cider-mode-hook ()
;;   ;; disable cider-refresh
;;   ;; call cider-refresh manually using M-x - when needed
;;   (define-key cider-mode-map (kbd "C-c C-x") nil))
;; (add-hook 'cider-repl-mode-hook 'my-cider-mode-hook)
;; minor modes for clojure-mode
(add-hook 'clojure-mode-hook #'enable-paredit-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'company-mode)

;;; ====  Clj-Refactor ==== ;;;
;; (require 'clj-refactor)
;; (defun clj-refactor-clojure-mode ()
;;     (clj-refactor-mode 1)
;;     (yas-minor-mode 1) ; for adding require/use/import statements
;;     ;; This choice of keybinding leaves cider-macroexpand-1 unbound
;;     (cljr-add-keybindings-with-prefix "C-c C-m"))
;; (add-hook 'clojure-mode-hook #'clj-refactor-clojure-mode)


;;; === Magit === ;;;
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)


;;; === Fiplr === ;;;
(require 'fiplr)
(global-set-key (kbd "C-c f f") 'fiplr-find-file)


;;; === Ace Window === ;;;
(require 'ace-window)
(global-set-key (kbd "C-x o") 'ace-window)


;;; ==== Golden ratio ==== ;;;
(require 'golden-ratio)
(golden-ratio-mode 0)
;; trigger golden-ratio
(add-to-list 'golden-ratio-extra-commands 'ace-window)
(add-to-list 'golden-ratio-extra-commands 'magit-status)


;;; === find-file-at-point === ;;;
(require 'ffap)
(global-set-key (kbd "C-c p") 'find-file-at-point)


;;; === GEISER === ;;;
(require 'geiser)
(setq geiser-mit-binary "scheme")
(setq geiser-active-implementations '(mit racket))
;; enable other minor modes for geiser repl
(add-hook 'geiser-repl-mode-hook #'enable-paredit-mode)
(add-hook 'geiser-repl-mode-hook #'rainbow-delimiters-mode)
;; enabble other minor modes for geiser (buffer)
(add-hook 'geiser-mode-hook #'enable-paredit-mode)
(add-hook 'geiser-mode-hook #'rainbow-delimiters-mode)


;;; ===  AG === ;;;
(require 'ag)
(setq ag-reuse-buffers 't)
(global-set-key (kbd "C-c a f") 'ag-files) ;; search [string] files [matching] in [dir]
(global-set-key (kbd "C-c a p") 'ag-project) ;; search [string] in project (.git)
(global-set-key (kbd "C-c a m") 'ag-project-files) ;; search [string] in project (.git) files [matching]


;;; ===  Multiple-Cursors === ;;;
(require 'multiple-cursors)
;; active region that spans multiple lines
(global-set-key (kbd "C-+") 'mc/edit-lines)
;; add multiple cursors based on keywords in the buffer
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-?") 'mc/mark-all-like-this)


;;; ===  NeoTree === ;;;
(require 'neotree)
(defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))
(global-set-key [f8] 'neotree-project-dir)


;;; ===  eyebrowse === ;;;
;; window configurations manager
(require 'eyebrowse)
(eyebrowse-mode t)


;;; === avy === ;;;
(require 'avy)
(global-set-key (kbd "C-;") 'avy-goto-char-timer)
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-2)


;;; === Markdown === ;;;
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;;; === Tiles === ;;;
(global-set-key (kbd "C-c SPC") 'tile)


;;; === Purescript ===
(require 'psc-ide)
(add-hook 'purescript-mode-hook
  (lambda ()
    (psc-ide-mode)
    (company-mode)
    (flycheck-mode)
    (turn-on-purescript-indentation)))
;; use exacutables relative to node_modules/.bin
(setq psc-ide-use-npm-bin t)
(setq psc-ide-add-import-on-completion 0)


;;; === Haskell === ;;;
;;(require 'intero)
;;(add-hook 'haskell-mode-hook 'intero-mode)
;; TODO (setq intero-blacklist '("~/code/haskell-project/"))
;; TODO (add-hook 'haskell-mode-hook 'intero-mode-whitelist)
;; (add-hook 'haskell-mode-hook 'indent-guide-mode)


;;; === Erlang === ;;;
(require 'erlang-start)


;;; === Company === ;;;
(use-package company
  :init
  (progn
    (setq company-idle-delay 0.3
    )
  )
  :bind (("M-TAB" . company-complete))
)


;;; === Helm === ;;;
(use-package helm
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (setq helm-candidate-number-limit           100
          helm-split-window-in-side-p           t     ; open helm buffer inside current window, not occupy whole other window
          helm-idle-delay                       0.0   ; update fast sources immediately (doesn't).
          helm-input-idle-delay                 0.01  ; this actually updates things reeeelatively quickly.
          helm-quick-update                     t ; do not display invisible candidates
          helm-M-x-requires-pattern             nil
          helm-ff-skip-boring-files             t
          helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
          helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
          helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
          helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
          helm-ff-file-name-history-use-recentf t
          helm-adaptive-history-file            "~/.emacs.d/helm-history"
    )
    (helm-mode))
  :bind (("C-c h" . helm-mini)
         ("C-h a" . helm-apropos)
         ("C-x C-b" . helm-buffers-list)
         ("C-x b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("C-x c o" . helm-occur)
         ;;("C-x c y" . helm-yas-complete)
         ;;("C-x c Y" . helm-yas-create-snippet-on-region)
         ("C-x c SPC" . helm-all-mark-rings))
)
;; (ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally

;;; better find ?
(use-package helm-swoop
  :bind
  (("C-x c s" . helm-swoop)
   ("C-x M-i" . helm-multi-swoop-all)
  )
  :config
  (progn
    ;; use I-Search then press M-i
    (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
    (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop))
)

;;;
(use-package helm-descbinds
  :defer t
  :bind (("C-h b" . helm-descbinds)
         ("C-h w" . helm-descbinds)))


(use-package smart-mode-line
  :init
  (sml/setup))

;;; === Undo tree visualiser === ;;;
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))


;;; === pop up help after a short delay === ;;;
(use-package guide-key
  :defer t
  :diminish guide-key-mode
  :config
  (progn
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c"))
  (guide-key-mode 1)))  ; Enable guide-key-mode


;;; === Navigation === ;;;
(use-package windmove
  :bind
  (("C-c w f" . windmove-right)
   ("C-c w b" . windmove-left)
   ("C-c w p". windmove-up)
   ("C-c w n" . windmove-down)
   ))

(bind-key "C-x p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)


;;; === look for symbol at point === ;;;
;; M-n, M-p
(use-package smartscan
  :defer t
  :config (global-smartscan-mode t))


(use-package expand-region
  :defer t
  :bind ("C-=" . er/expand-region))


;;; show git commit message at line
(use-package git-messenger
  :bind (("C-x v m" . git-messenger:popup-message)))
