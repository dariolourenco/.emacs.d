;----------------------;
;;; Custom Functions ;;;
;----------------------;

(defun system-is-linux ()
(interactive)
(string-equal system-type "gnu/linux"))

(defun make-plugin-path (plugin)
(expand-file-name
(concat plugin-path plugin)))


(defun make-elget-path (plugin)
(expand-file-name
(concat elget-path plugin)))

(defun include-elget-plugin (plugin)
(add-to-list 'load-path (make-elget-path plugin)))

(defun include-plugin (plugin)
(add-to-list 'load-path (make-plugin-path plugin)))

(provide 'custom-functions)

