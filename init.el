
(defvar package-archives)
(defvar package-archive-contents)
(defvar my:disabled-packages nil)

;;; Bootstrap
(setq package-archives '(("sunrise" . "http://joseito.republika.pl/sunrise-commander/")
                         ("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(load "~/.emacs.d/pre-startup.el" 'noerror)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))


;;; User package
(require 'use-package)

(defmacro user-package (name &rest args)
  "Wrapper over `use-package'.
Disables all packages that are member of the
`my:disabled-packages' list by injecting membership into
`use-package' :if keyword ."
  (declare (indent 1))
  (when (not (memq name my:disabled-packages))
    `(use-package ,name ,@args)))

(defconst user-package-font-lock-keywords
  '(("(\\(user-package\\)\\_>[ \t']*\\(\\sw+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-constant-face nil t))))

(font-lock-add-keywords 'emacs-lisp-mode user-package-font-lock-keywords)


;;; Environment

(let ((bindir (expand-file-name "~/bin")))
  (setenv "PATH" (concat bindir  ":" (getenv "PATH")))
  (add-to-list 'exec-path bindir))


;;; Packages and config


					;UI configs


(set-default-font "Inconsolata-14")

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(load-theme 'cyberpunk t)


(user-package scroll-bar
  :config (scroll-bar-mode -1))


(user-package tool-bar
  :config (tool-bar-mode -1))


(user-package menu-bar
  :bind ("M-k" . kill-this-buffer)
  :config (menu-bar-mode -1))



(user-package cus-theme
  :config
  (progn
    (user-package helm-themes
      :ensure helm-themes)
    (defun my:load-random-theme (&optional msg)
      "Load a random theme."
      (interactive "p")
      (let ((success))
        (while (not success)
          (let* ((themes (custom-available-themes))
                 (random-theme
                  (progn
                    (random t)
                    (nth (random (length themes)) themes))))
            (condition-case err
                (progn
                  (helm-themes--load-theme (symbol-name random-theme))
                  (setq success t))
              (error
               (message "Failed to load %s. Retrying..." random-theme)))
            (when (and success msg)
              (message "Loaded theme %s" random-theme))))))
    (user-package ample-theme
      :ensure ample-theme
      :defer t)
    (user-package color-theme-sanityinc-solarized
      :ensure color-theme-sanityinc-solarized
      :defer t)
    (user-package color-theme-sanityinc-tomorrow
      :ensure color-theme-sanityinc-tomorrow
      :defer t)
    (user-package cyberpunk-theme
      :ensure cyberpunk-theme
      :defer t)
    (user-package leuven-theme
      :ensure leuven-theme
      :defer t)
    (user-package monokai-theme
      :ensure monokai-theme
      :defer t)
    (user-package zenburn-theme
      :ensure zenburn-theme
      :defer t)))

					; Packages

(user-package ace-jump-mode
  :if (not noninteractive)
  :bind (("C-c SPC" . ace-jump-mode)
         ("C-c C-SPC" . ace-jump-mode-pop-mark))
  :ensure ace-jump-mode
  :config
  (progn
    (setq ace-jump-mode-case-fold t)
    (ace-jump-mode-enable-mark-sync)
    (setq ace-jump-mode-submode-list
          '(ace-jump-word-mode ace-jump-char-mode ace-jump-line-mode))))


					;silversearcher
(user-package ag
  :if (not noninteractive)
  :ensure ag)

(user-package alert
  :ensure alert
  :config (setq alert-default-style 'libnotify))

					;autocomplete
(user-package auto-complete
  :if (not noninteractive)
  :ensure auto-complete
  :diminish auto-complete-mode
  :config (progn
            (require 'auto-complete-config)
            (ac-config-default)
            (setq-default ac-sources '(ac-source-yasnippet
                                       ac-source-filename
                                       ac-source-abbrev
                                       ac-source-dictionary
                                       ac-source-words-in-same-mode-buffers))
            (global-auto-complete-mode 1)))

(user-package dired-details
  :if (not noninteractive)
  :ensure dired-details
  :config (progn
            (dired-details-install)))

(user-package ido
  :if (not noninteractive)
  :config
  (progn
    (user-package ido-vertical-mode
      :ensure ido-vertical-mode)
    (user-package flx
      :ensure flx)
    (user-package flx-ido
      :ensure flx-ido)
    (setq ido-enable-flex-matching t
	  ido-use-faces nil
	  flx-ido-use-faces t)
    (ido-mode 1)
    (ido-everywhere 1)
    (ido-vertical-mode 1)
    (flx-ido-mode 1)))

(user-package flycheck
  :ensure flycheck
  :config
  (progn
    ;; Add virtualenv support for checkers
    (defadvice flycheck-check-executable
	(around python-flycheck-check-executable (checker)
		activate compile)
      "`flycheck-check-executable' with virtualenv support."
      (if (eq major-mode 'python-mode)
	  (let* ((process-environment (python-shell-calculate-process-environment))
		 (exec-path (python-shell-calculate-exec-path)))
	    ad-do-it)
	ad-do-it))
    (defadvice flycheck-start-checker
	(around python-flycheck-start-checker (checker)
		activate compile)
      "`flycheck-start-checker' with virtualenv support."
      (if (eq major-mode 'python-mode)
	  (let* ((process-environment (python-shell-calculate-process-environment))
		 (exec-path (python-shell-calculate-exec-path)))
	    ad-do-it)
	ad-do-it))
    (setq flycheck-mode-line-lighter " ")
    (global-flycheck-mode 1)
    (user-package helm-flycheck
      :ensure helm-flycheck)
    (bind-key "C-c ! !" #'helm-flycheck flycheck-mode-map)))

(user-package markdown-mode
  :if (not noninteractive)
  :ensure markdown-mode)


(user-package multi-web-mode
  :if (not noninteractive)
  :ensure multi-web-mode
  :config (progn
	    (setq mweb-default-major-mode 'html-mode)
	    (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
			      (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
			      (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
	    (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
	    (multi-web-global-mode 1)))


(user-package js
  :if (not noninteractive)
  :config
  (progn
    (user-package jquery-doc
      :ensure jquery-doc)
    (add-hook 'js-mode-hook 'jquery-doc-setup)))


(user-package css-mode
  :if (not noninteractive)
  :ensure css-mode
  :config (setq css-indent-offset 2))


(user-package php-mode
  :if (not noninteractive)
  :ensure php-mode)


(user-package paren
  :config (show-paren-mode 1))



(user-package powerline
  :if (not noninteractive)
  :ensure powerline
  :config (powerline-default-theme))


(user-package repeat
  :if (not noninteractive)
  :bind ("C-z" . repeat))


(user-package rainbow-mode
  :if (not noninteractive)
  :ensure rainbow-mode
  :config (progn
	    (mapc (lambda (mode)
		    (add-to-list 'rainbow-r-colors-major-mode-list mode))
		  '(css-mode emacs-lisp-mode lisp-interaction-mode))
	    (add-hook 'prog-mode-hook #'rainbow-turn-on)))



(user-package rainbow-delimiters
  :if (not noninteractive)
  :ensure rainbow-delimiters
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


(user-package undo-tree
  :if (not noninteractive)
  :diminish undo-tree-mode
  :bind ("C-x _" . undo-tree-visualize)
  :ensure undo-tree
  :config (global-undo-tree-mode 1))


(user-package smartparens
  :if (not noninteractive)
  :ensure smartparens
  :diminish (smartparens-mode . " π")
  :config (progn
	    (--each sp--html-modes
	      (eval-after-load (symbol-name it) '(require 'smartparens-html)))
	    (eval-after-load "latex" '(require 'smartparens-latex))
	    (eval-after-load "tex-mode" '(require 'smartparens-latex))
	    (sp-pair "'" nil :unless '(sp-point-after-word-p))
	    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
	    (sp-with-modes '(markdown-mode rst-mode)
	      (sp-local-pair "*" "*" :bind "C-*")
	      (sp-local-tag "2" "**" "**")
	      (sp-local-tag "s" "```scheme" "```")
	      (sp-local-tag "<" "<_>" "</_>" :transform 'sp-match-sgml-tags))
	    (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
	      (sp-local-tag "i" "\"<" "\">"))
	    (sp-with-modes '(html-mode sgml-mode)
	      (sp-local-pair "<" ">"))

	    (sp-with-modes sp--lisp-modes
	      ;; disable ', it's the quote character!
	      (sp-local-pair "'" nil :actions nil)
	      ;; also only use the pseudo-quote inside strings where it serve as
	      ;; hyperlink.
	      (sp-local-pair "`" "'" :when '(sp-in-string-p))
	      (sp-local-pair "(" nil :bind "M-("))
	    (add-hook 'smartparens-enabled-hook
		      (lambda ()
			(when (memq major-mode sp--lisp-modes)
			  (smartparens-strict-mode 1))))
	    (smartparens-global-mode 1)
	    (show-smartparens-global-mode 1)))

(user-package ibuffer
  :if (not noninteractive)
  :bind ("C-x C-b" . ibuffer)
  :config
  (progn
    (setq ibuffer-show-empty-filter-groups nil
          ibuffer-saved-filter-groups
          (list (append
                 (cons "default"
                       ;; Generate filters by major modes from the
                       ;; auto-mode-alist
                       (let ((mode-filters))
                         (dolist (element auto-mode-alist)
                           (when (ignore-errors (fboundp (cdr element)))
                             (let* ((mode (cdr element))
                                    (name (if (string-match "\\(-mode\\)?\\'"
                                                            (symbol-name mode))
                                              (capitalize
                                               (substring (symbol-name mode)
                                                          0 (match-beginning 0)))
                                            (symbol-name mode))))
                               (when (not (assoc-string name mode-filters))
                                 (setq mode-filters
                                       (cons (list name (cons 'mode mode))
                                             mode-filters))))))
                         mode-filters))
                 ;; Custom added filters.
                 '(("Magit" (name . "^\\*magit"))
                   ("Irc" (mode . rcirc-mode))
                   ("Css" (mode . scss-mode))
                   ("W3m" (name . "^\\*w3m"))))))
    (add-hook 'ibuffer-mode-hook
              (lambda ()
                (ibuffer-switch-to-saved-filter-groups "default")))))

(user-package helm
  :if (not noninteractive)
  :ensure helm
  :config
  (progn
    (bind-key "<RET>" #'helm-maybe-exit-minibuffer helm-map)
    (bind-key "C-i" #'helm-execute-persistent-action helm-map)
    (bind-key "C-j" #'helm-maybe-exit-minibuffer helm-map)
    (bind-key "C-x b" #'helm-mini)
    (bind-key "C-x C-f" #'helm-find-files)
    (bind-key "C-z" #'helm-select-action helm-map)
    (bind-key "M-y" #'helm-show-kill-ring)
    (setq helm-buffers-fuzzy-matching t
          helm-ff-auto-update-initial-value t
          helm-ff-file-name-history-use-recentf t
          helm-ff-search-library-in-sexp t
          helm-ff-skip-boring-files t
          helm-move-to-line-cycle-in-source t
          helm-scroll-amount 8
          helm-split-window-in-side-p t)
    (helm-mode 1)
    (user-package helm-swoop
      :ensure helm-swoop)
    (bind-key "C-x M-i" #'helm-multi-swoop)
    (bind-key "M-I" #'helm-swoop-back-to-last-point)
    (bind-key "M-i" #'helm-multi-swoop-all-from-helm-swoop helm-swoop-map)
    (bind-key "M-i" #'helm-swoop)
    (bind-key "M-i" #'helm-swoop-from-isearch isearch-mode-map)
    (setq helm-multi-swoop-edit-save t
          helm-swoop-speed-or-color t
          helm-swoop-split-direction #'split-window-horizontally
          helm-swoop-split-with-multiple-windows nil
          helm-swoop-use-line-number-face t)))

(setq inhibit-startup-screen t
      initial-scratch-message ""
      x-select-enable-clipboard t
      x-select-enable-primary t)
(load "~/.emacs.d/secrets.el" 'noerror)
(load "~/.emacs.d/post-startup.el" 'noerror)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(provide 'init)

;;; init ends here
