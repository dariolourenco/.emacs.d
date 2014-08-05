;-------------------;
;;; Auto-Complete ;;;
;-------------------;


(setq ac-directory (make-elget-path "auto-complete"))
(add-to-list 'load-path ac-directory)
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories (concat ac-directory "ac-dict"))
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode 1)

(provide 'auto-complete-settings)
