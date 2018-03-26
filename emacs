;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defun load-amce (name)
  (let ((full-name (concat "~/.emacs.d/amce/" name ".el"))  )
    (load-file full-name)))

(defun rec-load-amce (files)
  (when files
    (load-amce (car files))
    (rec-load-amce (cdr files))))

(setq files '("theme" "global" "defuns" "disabled" "fonts" "cua" "html" "shell" "ido" "packages"))

(rec-load-amce files)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-cljs-boot-repl
   "(do (require 'adzerk.boot-cljs-repl) (adzerk.boot-cljs-repl/start-repl))")
 '(cider-cljs-lein-repl
   "(do (require 'figwheel-sidecar.repl-api) (figwheel-sidecar.repl-api/start-figwheel!) (figwheel-sidecar.repl-api/cljs-repl))")
 '(company-ghc-show-info t)
 '(geiser-mit-binary "scheme" t)
 '(haskell-process-type (quote stack-ghci))
 '(package-selected-packages
   (quote
    (dockerfile-mode yaml-mode intero 0blayout psci psc-ide repl-toggle flycheck-purescript purescript-mode company-ghc flycheck flycheck-haskell flymake-haskell-multi scala-mode gherkin-mode geiser racket-mode clj-refactor ace-window ido-vertical-mode rainbow-mode restclient json-mode markdown-mode rust-mode elm-mode go-mode helm magit golden-ratio haskell-mode company cider clojure-mode-extra-font-locking clojure-mode rainbow-delimiters paredit exec-path-from-shell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "Red"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "orange2"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow2"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "green2"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "blue1"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "dark magenta"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "brown")))))
