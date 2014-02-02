;------------------------;
;;;       Python       ;;;
;------------------------;

(require 'python)

;; ; jedi python completion
(include-elget-plugin "ctable") ; required for epc
(include-elget-plugin "deferred") ; required for epc
(include-elget-plugin "epc") ; required for jedi
(include-elget-plugin "jedi")
(require 'jedi)
(setq jedi:setup-keys t)
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)


(provide 'python-settings)


