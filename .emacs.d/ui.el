;;; ui --- Colors!
;;; Commentary:
;;; bleh
;;;
;;; Code:

;; Color for lines, indentation and column
(setq line-backcolor "#eff")
(setq line-forecolor "#000")
(setq col-highlight-color "#ddd")
(setq comment-background-color "#222")
(setq comment-color "#fff")

;; Color for long lines.
(setq warning-color "#f00")

;; Matching parentheses.
(setq show-paren-style 'expression)
(show-paren-mode 1)

;; Disable menus
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Neat mode line
(use-package smart-mode-line
  :config
  (setq sml/theme 'light)
  (smart-mode-line-enable)
  )

;; Smoother scroll
(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1)

;; Theme
;; download and package-install-file:
;; https://melpa.org/packages/tao-theme-20160330.650.tar
;; (load-theme 'tao-yin t)

;; (use-package tao-theme
;;    :config (load-theme 'tao-yin t))

;; Nice theme.
;; (use-package pastelmac-theme
;;    :config
;;    (load-theme 'pastelmac t))

;; (use-package moe-theme
;;   :config
;;   (moe-light))

;; (use-package zenburn-theme ????
;;    :config
;;    (load-theme 'zenburn t))

;; Comment colors
(set-face-background 'font-lock-comment-face comment-background-color)
(set-face-background 'font-lock-comment-delimiter-face comment-background-color)
(set-face-foreground 'font-lock-comment-face comment-color)

;; Mode line settings.
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;;; See trailing whitespace.
(use-package whitespace
  :config
  (setq whitespace-style '(face trailing))
  (global-whitespace-mode))

; Line numbering.
(use-package linum
  :config (global-linum-mode 1))

(use-package linum-relative
  :config
  (linum-relative-global-mode)
  ;; Format numbers according to # of lines in file.
  (custom-set-variables
   '(linum-relative-current-symbol "")
   '(linum-relative-format
     (let ((max-width
	   (length (number-to-string (count-lines (point-min) (point-max))))
	  )) (format "%%%ds " max-width)))))


;; Highlight color literals
(use-package rainbow-mode
  :config (autoload 'rainbow-mode "rainbow" "Highlight color literals"))

;; Highlight current line.
(global-hl-line-mode 1)
(set-face-background 'hl-line line-backcolor)
(set-face-foreground 'hl-line line-forecolor)

;; Highlight current column and line.
(use-package col-highlight
  :config
  (set-face-background 'col-highlight col-highlight-color)
  (column-highlight-mode))

;; Highlight columns 79 and 80.
;; Test line: ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package column-marker
  :config
  (set-face-background 'column-marker-1 warning-color)
  (set-face-background 'column-marker-2 warning-color)
  (let ((setup-column-marker
	(lambda () (interactive)
		(column-marker-1 79)
		(column-marker-2 80))))
  (add-hook 'LaTeX-mode-hook setup-column-marker)
  (add-hook 'prog-mode-hook setup-column-marker)))

;; Mark lines using output from git diff.
(use-package git-gutter
  :defer 0.5
  :config
  (global-git-gutter-mode +1)
  (git-gutter:linum-setup))

;;; ui.el ends here
