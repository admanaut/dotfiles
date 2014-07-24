;; Setup for Flymake code checking.
(require 'flymake)
;;(load-library "flymake-cursor")

;; Script that flymake uses to check code. This script must be
;; present in the system path.
(setq pycodechecker "/usr/local/bin/pychecker")

(when (load "flymake" t)
  (defun flymake-pycodecheck-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list pycodechecker (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pycodecheck-init)))

(add-hook 'python-mode-hook 'flymake-mode)

;;/usr/local/bin/pychecker
;;
;;#!/bin/bash
;;
;;#pyflakes "$1"
;;pep8 --ignore=E221,E701,E202 --repeat "$1"
;;true
