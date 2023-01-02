;;; init.el --- Load packages fast! Configure emacs, real nice!!!
;;;
;;; Commentary:
;;; lots of use-package
;;;
;;; Author:
;;; Hugo O. Rivera
;;;
;;; Code:

;; Install use-package.
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("stable-melpa" . "https://stable.melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t)

;; Wrap in a let statement to prevent crazy regexes from running on
;; every .el and .elc file loaded on startup
(let ((file-name-handler-alist nil))

;; Increase garbage collection threshold when loading init file, reset when done.
(setq gc-cons-threshold 100000000)
(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold 100000)
   ;; (message "gc-cons-threshold restored to %S" gc-cons-threshold)
 )))


;; Measure startup time
;; (use-package benchmark-init
;;   :ensure t
;;   :config (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Cleanup buffers that have been inactive for more than 3 days
;; M-x clean-buffer-list
(use-package midnight)

;; Syntax checker for ~40 languages.
;; pip install --user proselint
(use-package flycheck
  :config
  (global-flycheck-mode)
  (setq flycheck-indication-mode 'left-fringe)

  ;; Navigate errors using Ctrl-J and Ctrl-K
  (global-set-key (kbd "C-j") 'flycheck-next-error)
  (global-set-key (kbd "C-k") 'flycheck-previous-error)

  ;; Window size
  (add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
              (display-buffer-reuse-window
               display-buffer-in-side-window)
              (side            . bottom)
              (window-height   . 0.25))))

;; Show flycheck errors/warnings inline
(use-package flycheck-inline
  :config
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-inline-mode)))

;; Clean up whitespace.
(use-package whitespace-cleanup-mode)

;; Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config (setq markdown-header-scaling t))

;; Vim keybindings.
(use-package evil
  :init
  ;; Free the Ctrl-Z binding
  (setq evil-toggle-key "C-`")
  :config
  (evil-mode)
  ;; Bind ; to :
  (define-key evil-normal-state-map (kbd ";") 'evil-ex)
  ;; Change cursor color depending on mode
  (setq evil-emacs-state-cursor '("red" box))
  (setq evil-normal-state-cursor '("green" box))
  (setq evil-visual-state-cursor '("orange" box))
  (setq evil-insert-state-cursor '("red" bar))
  (setq evil-replace-state-cursor '("red" bar))
  (setq evil-operator-state-cursor '("red" hollow)))

;; HELM
(use-package helm
  :defer 1
  :config
  (setq helm-autoresize-max-height 0
        helm-autoresize-min-height 20
        helm-split-window-in-side-p t)
  (helm-autoresize-mode 1)
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (helm-mode))

(use-package helm-projectile
  :config
  (helm-projectile-on))

;; ;; IDO shows list of choices as you type
;; (use-package ido
;;   :config
;;   (ido-mode t)
;;   (ido-everywhere 1)
;;   )
;;
;; ;; Fuzzy matching for IDO
;; (use-package flx-ido
;;   :after (ido)
;;   :config
;;   (flx-ido-mode 1)
;;   ;; Don't do fuzzy matching until collection is this small
;;   (setq flx-ido-threshold 5000)
;;   ;; Highlight matches
;;   (setq ido-enable-flex-matching t)
;;   (setq ido-use-faces nil))
;;
;; ;; Smart M-x enhancement built on top of IDO
;; (use-package smex
;;   :defer 0.5
;;   :config
;;   (smex-initialize)
;;   ;; (global-set-key (kbd "M-x") 'smex)
;;   ;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)
;;   )

;; Elm
(use-package elm-mode
  :defer t)

;; Python helper
;; pip install --user autopep8 yapf flake8 jedi black rope elpy
(use-package elpy
  :ensure t
  :defer t
  :init (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")
  ;; Format Python code before saving
  (add-hook 'before-save-hook 'elpy-format-code))

;; Jupyter notebook editor and viewer
;; Start EIN using ONE of the following:
;;
;;     Open an .ipynb file normally in emacs and press C-c C-o, or,
;;     M-x ein:run launches a jupyter process from emacs, or,
;;     M-x ein:login to a running jupyter server
;;
;; Use C-u M-x ein:login for services such as mybinder.org requiring cookie authentication.
(use-package ein)

(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :init (setq pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended)
  :config (add-hook 'python-mode-hook #'pipenv-mode))

;; Sort imports in Python buffers
;; pip install --user isort
(use-package py-isort
  :defer t
  ;; Sort on save
  :config (add-hook 'before-save-hook 'py-isort-buffer))

;; Wrapping and syntax highlighting for docstrings in both reStructuredText
;; and Epydoc formats
(use-package python-docstring
  :defer 1
  :config (python-docstring-mode))

;; Coffeescript mode.
(use-package coffee-mode
  :mode "\\.coffee\\'"
  :defer 1)

;; JSON mode.
(use-package json-mode
  :defer 1)

;; Improved Haskell mode.
;; (use-package intero
;;   :defer 1
;;   :no-require t)

;; Markdown mode.
(use-package markdown-mode
  :config
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

;; Cleans trailing whitespace if it was clean when the buffer was opened
(use-package whitespace-cleanup-mode
  :defer 1
  :config (global-whitespace-cleanup-mode))

;; Git porcelain
(use-package magit
  :defer 1
  :no-require t)

;; Org mode. Enable indent mode.
(use-package org
  :defer 1
  :no-require t
  :config
  (setq org-startup-indented 'false))

;; Projectile project interaction library
(use-package projectile
  :defer 1
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-p") 'projectile-find-file)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; File-tree
;; Activate using neotree-toggle
(use-package neotree
  :defer 1)

(use-package ranger
  :ensure t
  :config
  (setq ranger-show-hidden t)
  ;; Replace dired
  (ranger-override-dired-mode t)
  (setq ranger-override-dired 'ranger))

;; Icons package
;; Run all-the-icons-install-fonts to auto-magically install fonts.
(use-package all-the-icons
  :defer 1)

;; For editing JS and HTML
(use-package web-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

  :config
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

  (setq web-mode-markup-indent-offset 4)

  (flycheck-add-mode 'javascript-eslint 'web-mode))

;; Auto-completion framework
(use-package company
  :config
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0)
  (setq company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend))

  ;; Show completions by pressing tab
  (define-key company-mode-map [remap indent-for-tab-command] #'company-indent-or-complete-common))

;; Show
;; Does not work when emacs is running in a terminal
(use-package company-quickhelp
  :after (company)
  :config (company-quickhelp-mode))

;; Offer completions for header file names in C++, C, and ObjC modes
(use-package company-c-headers
  :after (company)
  :config (add-to-list 'company-backends 'company-c-headers))

;; Use tern to get helpful JS completion
;; (use-package company-tern
;;   :after (company)
;;   :config
;;   (add-to-list 'company-backends 'company-tern)
;;   (add-hook 'web-mode-hook (lambda () (tern-mode) (company-mode))))

;; Use jedi to get Python completions
(use-package company-jedi
  :after (company)
  :config
  (add-to-list 'company-backends 'company-jedi))

;; Use local node project's node_modules
(use-package add-node-modules-path
  :config (add-hook 'flycheck-mode-hook 'add-node-modules-path))

;; Paste from system clipboard with Ctrl-Shift-V
(global-set-key (kbd "C-S-v") 'clipboard-yank)

;; Mouse mode
(xterm-mouse-mode 1)

;; No splash screen. Show agenda on startup.
(setq inhibit-splash-screen t)

;; Enable on-the-fly spellcheck suggestions
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(with-eval-after-load "ispell"
        (setq ispell-program-name "hunspell")
        (setq ispell-dictionary "en_US,es,es_MX")
        (ispell-set-spellchecker-params)
        (ispell-hunspell-add-multi-dic "en_US,es,es_MX")
)

;; Show spelling corrections in popup.
(use-package flyspell-popup
  :defer 1
  :config
  (add-hook 'flyspell-mode-hook #'flyspell-popup-auto-correct-mode)
  (setq flyspell-popup-correct-delay 1))

;; Do not clobber
(global-auto-revert-mode 1)

;; Enable Unicode.
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; C indentation
(setq-default c-basic-offset 4)
(setq c-basic-offset 4)

;; .check <=> .c
(add-to-list 'auto-mode-alist '("\\.check\\'" . c-mode))

;; .m should enable octave mode
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; Enable y/n answers.
(fset 'yes-or-no-p 'y-or-n-p)

;; Automatic indentation.
(electric-indent-mode 1)

;; No hard tabs.
(setq-default indent-tabs-mode nil)

;; Backup and auto-save directories.
(defvar backup-dir (concat user-emacs-directory "emacs-backups/"))
(defvar autosave-dir (concat user-emacs-directory "emacs-autosaves/"))

(if (not (file-exists-p backup-dir))
    (make-directory backup-dir t))

(if (not (file-exists-p autosave-dir))
    (make-directory autosave-dir t))

(setq backup-directory-alist `((".*" . ,backup-dir)))
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq auto-save-list-file-prefix (concat autosave-dir ".saves-"))

;; Backup settings
(setq make-backup-files t    ; backup of a file the first time it is saved.
      backup-by-copying t    ; don't clobber symlinks
      version-control t      ; version numbers for backup files
      delete-old-versions t  ; delete excess backup files silently
      kept-old-versions 15   ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 15   ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t    ; auto-save every buffer that visits a file
      auto-save-timeout 2    ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 5   ; number of keystrokes between auto-saves (default: 300)
)

(setq-default explicit-shell-file-name "/bin/bash")
(setq-default shell-file-name "/bin/bash")

;;(load "~/.emacs.d/PG/generic/proof-site.el")

;; Load manually installed packages.
;; (load-file "~/.emacs.d/emacs-local-packages/proofgeneral-4.2/generic/proof-site.el")

;; Better proof point.
;; (add-hook 'proof-mode-hook
;; 	  (lambda () (local-set-key (kbd "C-c C-RET") 'proof-goto-point)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("04232a0bfc50eac64c12471607090ecac9d7fd2d79e388f8543d1c5439ed81f5" "065efdd71e6d1502877fd5621b984cded01717930639ded0e569e1724d058af8" default))
 '(linum-relative-current-symbol "")
 '(linum-relative-format
   (let
       ((max-width
         (length
          (number-to-string
           (count-lines
            (point-min)
            (point-max))))))
     (format "%%%ds " max-width)))
 '(org-agenda-files '("~/org/"))
 '(org-modules
   '(org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-protocol org-rmail org-w3m org-drill))
 '(package-selected-packages
   '(company-c-headers pipenv smooth-scrolling ein helm zenburn diff-hl fill-column-indicator cursor-chg ergoemacs-mode org-contrib flyspell-popup org-drill xresources-theme column-marker git-gutter android-mode whitespace-cleanup-mode use-package tao-theme smex rainbow-mode magit linum-relative langtool json-mode impatient-mode haskell-mode guide-key flycheck evil col-highlight coffee-mode benchmark-init auto-complete auctex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Load UI configuration.
(load "~/.emacs.d/ui.el")

;;; init.el ends here
