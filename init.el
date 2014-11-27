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

(user-package ag
  :if (not noninteractive)
  :ensure ag)

(user-package alert
  :ensure alert
  :config (setq alert-default-style 'libnotify))

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
(user-package css-mode
  :if (not noninteractive)
  :ensure css-mode
  :config (setq css-indent-offset 2))

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
