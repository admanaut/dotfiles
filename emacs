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
 )
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
