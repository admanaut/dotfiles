(defun load-amce (name)
  (let ((full-name (concat "~/.emacs.d/amce/" name ".el"))  )
    (load-file full-name)))

(defun rec-load-amce (files)
  (when files
    (load-amce (car files))
    (rec-load-amce (cdr files))))

(setq files '("theme" "global" "defuns" "disabled" "fonts" "cua" "html" "shell" "ido" "packages"))

(rec-load-amce files)
