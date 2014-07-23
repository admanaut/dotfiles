(add-to-list 'load-path "~/.emacs.d/vendor")

;;(setq custom-file "~/.emacs.d/amce/custom.el")
;;(load custom-file 'noerror)

(load "amce/theme")
(load "amce/global")
(load "amce/defuns")
(load "amce/disabled")
(load "amce/fonts")
(load "amce/cua")
(load "amce/html")
(load "amce/shell")

(vendor 'web-mode)
