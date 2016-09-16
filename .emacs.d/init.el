;;; init.el --- Load packages fast!
;;;
;;; Commentary:
;;;
;;; uses use-package
;;;
;;; Code:

;; Install use-package.
(require 'package)
(setq package-enable-at-startup nil)
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

;; Higher garbage collection limit while loading the init file.
(setq gc-cons-threshold 100000000)

;; No hard tabs.
(setq-default indent-tabs-mode nil)

(use-package benchmark-init
  :config (benchmark-init/activate)
)

;; Syntax checker for ~40 languages.
(use-package flycheck
  :config (global-flycheck-mode))

;; Clean up whitespace.
(use-package whitespace-cleanup-mode)

;; LaTeX.
(use-package tex-site :ensure auctex)

;; PDF preview for LaTeX.
;; (use-package latex-preview-pane)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


;; Vim keybindings.

(use-package evil
  :config
  (evil-mode)
  ;; Bind ; to :
  (define-key evil-normal-state-map (kbd ";") 'evil-ex))

;; IDO auto complete.
(use-package ido
  :config (ido-mode t))

(use-package smex
  :config
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))

;; Web mode.
;; (use-package web-mode
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode)))

;; Live web development.
;; Commands: httpd-start to start server
;;           impateint-mode to publish file
(use-package impatient-mode)

;; Javascript mode.
;; (use-package js2-mode)

;; Coffeescript mode.
(use-package coffee-mode)

;; JSON mode.
(use-package json-mode)

;; Improved Haskel mode.
(use-package intero
  :no-require t)

;; Markdown mode.
;; (use-package markdown-mode
;;   :config
;;   (autoload 'markdown-mode "markdown-mode"
;;     "Major mode for editing Markdown files" t)
;;   (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
;;   (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
;;   (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

;; Explore keybindings.
(use-package guide-key
  :config
  (setq guide-key/guide-key-sequence '("C-x"))
  (setq guide-key/recursive-key-sequence-flag t)
  (setq guide-key/idle-delay 0.1)
  (guide-key-mode 1))

;; See trailing whitespace.
(use-package whitespace
  :config
  (setq whitespace-style '(face trailing))
  (global-whitespace-mode))

(use-package whitespace-cleanup-mode
  :config (global-whitespace-cleanup-mode))

;; Git.
(use-package magit
  :no-require t)

;; Auto complete.
(use-package auto-complete
  :config (ac-config-default))

;; Android
(use-package android-mode)

;; System monitor.
;; (use-package symon
;;   :config (symon-mode))

;; Copy with your mouse.
;; (use-package mouse-copy
;;   :config
;;   (global-set-key [C-down-mouse-1] 'mouse-drag-secondary-pasting)
;;   (global-set-key [C-S-down-mouse-1] 'mouse-drag-secondary-moving))

;; Org mode. Enable indent mode.
(use-package org
  :no-require t
  :config (add-hook 'org-mode-hook 'org-indent-mode))

;; No splash screen. Show agenda on startup.
(setq inhibit-splash-screen t)
;; (add-hook 'after-init-hook (lambda ()
;; 			   (org-agenda-list)
;;      		   (delete-other-windows)))

;; Save us from the clobbering.
(global-auto-revert-mode 1)

;; Enable Unicode.
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
 '(org-agenda-files
   (quote
    ("~/TODO.org" "~/107TODO.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Better proof point.
(add-hook 'proof-mode-hook
	  (lambda () (local-set-key (kbd "C-c C-RET") 'proof-goto-point)))

;; Popup spelling corrections.
;; (use-package flyspell-popup
;;   :config (add-hook 'flyspell-mode-hook #'flyspell-popup-auto-correct-mode))

;; Use aspell.
;; (setq ispell-program-name "aspell")
;; (setq ispell-extra-args '("--sug-mode=ultra"))
;; (setq ispell-list-command "--list")
;;
;; ;; Spanish dictionary
;; (setq ispell-dictionary "espanol")

;; Use java based LanguageTool grammar and spellchecker
(use-package langtool
  :config
  (setq langtool-language-tool-jar "~/.emacs.d/LanguageTool-3.3/languagetool-commandline.jar")
  (setq langtool-default-language "es")
  (setq langtool-mother-tongue "en"))


;; C indentation
(setq-default c-basic-offset 4)
(setq c-basic-offset 4)

;; .check <=> .c
(add-to-list 'auto-mode-alist '("\\.check\\'" . c-mode))

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
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      kept-old-versions 15              ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 15              ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 2              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 5             ; number of keystrokes between auto-saves (default: 300)
)

;; Load UI configuration.
(load "~/.emacs.d/ui.el")

;; Load manually installed packages.
;; (load-file "~/.emacs.d/emacs-local-packages/proofgeneral-4.2/generic/proof-site.el")

;; Reset garbage collection limit to a sane number.
(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold 1000000)
   (message "gc-cons-threshold restored to %S"
            gc-cons-threshold)))
)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
 '(org-agenda-files (quote ("~/TODO.org")))
 '(package-selected-packages
   (quote
    (android-mode whitespace-cleanup-mode use-package tao-theme smex rainbow-mode magit linum-relative langtool json-mode impatient-mode haskell-mode guide-key flycheck evil column-marker col-highlight coffee-mode benchmark-init auto-complete auctex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
