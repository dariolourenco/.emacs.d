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
(setq ring-bell-function 'ignore)      

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
                         
;; el-get
(include-plugin "el-get")
(require 'el-get)
(package-initialize)
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
      
;; LaTeX and Auctex
(require 'latex-settings)      
      
      
      
;---------------------------------------------------------------------
;; Put auto 'custom' changes in a separate file (this is stuff like
;; custom-set-faces and custom-set-variables)
(load
 (setq custom-file (expand-file-name "settings/custom.el" user-emacs-directory))
 'noerror)
