;;
;; guide-key displays the available key bindings automatically and dynamically
;; https://github.com/kai2nenobu/guide-key
;;
(require 'guide-key)
;; prefix key
(setq guide-key/guide-key-sequence '("C-x"))
(setq guide-key/idle-delay 0.5)
(setq guide-key/highlight-command-regexp
      '("rectangle"
        ("register" . font-lock-type-face)
        ("bookmark" . "hot pink")))

(guide-key-mode 1)
