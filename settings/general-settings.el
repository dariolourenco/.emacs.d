;--------------------------------;
;;; General or Global Settings ;;;
;--------------------------------;

; set PATH, because we don't load .bashrc
; function from https://gist.github.com/jakemcc/3887459
(defun set-exec-path-from-shell-PATH ()
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo -n $PATH'")))
(setenv "PATH" path-from-shell)
(setq exec-path (split-string path-from-shell path-separator))))
(if window-system (set-exec-path-from-shell-PATH))
; language
(setq current-language-environment "English")


(provide 'general-settings)