;; path where settings files are kept
(add-to-list 'load-path "~/.emacs.d/settings")

;; global config variables
(setq plugin-path "~/.emacs.d/plugins/")
(setq elget-path "~/.emacs.d/el-get/")


;; various generic/global config
(require 'custom-functions)
(require 'general-settings)

;----------------------;
;; Additional repos
;; marmelade repo
;----------------------;

(require 'package)
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))

;; el-get
(include-plugin "el-get")
(require 'el-get)

;----------------------;
;       Utilities      ;
;----------------------;

;; Popup
(include-elget-plugin "popup")
(require 'popup)

;; Auto complete
(require 'auto-complete-settings)      

;; Websocket
(include-plugin "websocket")
(require 'websocket)

;; Request
(include-plugin "request")
(require 'request)

;; Camelcase functions
(require 'camelcase-settings)

;---------------;
;    MODES      ;
;---------------;

;; Ido mode
(require 'ido)
(ido-mode 1)

;; Python mode
(require 'python-settings)
      
;; Markdown mode
(require 'markdown-settings)      
      
      
      
      
      
      
;---------------------------------------------------------------------
;; Put auto 'custom' changes in a separate file (this is stuff like
;; custom-set-faces and custom-set-variables)
(load
 (setq custom-file (expand-file-name "settings/custom.el" user-emacs-directory))
 'noerror)