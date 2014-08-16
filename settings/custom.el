(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; color theme
(add-to-list 'custom-theme-load-path (make-plugin-path "color-theme-solarized"))
(load-theme 'solarized-dark 1)
(setq solarized-termcolors 256)

(set-default-font "Inconsolata-13")








