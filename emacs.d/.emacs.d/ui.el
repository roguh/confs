;;; ui --- Colors!
;;; Commentary:
;;; UI configuration
;;;
;;; Code:

;; Theme
;; (use-package ample-theme
;;   :ensure t
;;   :init (progn (load-theme 'ample t t)
;;                (load-theme 'ample-flat t t)
;;                (load-theme 'ample-light t t)
;;                (enable-theme 'ample-flat)))

(use-package zenburn-theme
  :ensure t
  :init
  (load-theme 'zenburn t)
  (setq zenburn-use-variable-pitch t)
  ;; scale headings in org-mode
  (setq zenburn-scale-org-headlines t)
  ;; scale headings in outline-mode
  (setq zenburn-scale-outline-headlines t))

;; Matching parentheses.
(show-paren-mode 1)

;; Disable menus
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Smoother scroll
(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1)

;; Mode line settings.
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;;; See trailing whitespace.
(use-package whitespace
  :config
  (setq whitespace-style '(face trailing))
  (global-whitespace-mode))

; Relative line numbering.
(use-package linum-relative
  :config
  (linum-relative-global-mode))

;;  ;; Format numbers according to # of lines in file.
;;  (custom-set-variables
;;   '(linum-relative-current-symbol "")
;;   '(linum-relative-format
;;     (let ((max-width
;;	   (length (number-to-string (count-lines (point-min) (point-max))))
;;	  )) (format "%%%ds " max-width)))))


;; Highlight current line.
(global-hl-line-mode 1)

(defconst line-backcolor "#eff")
(defconst line-forecolor "#000")
(set-face-background 'hl-line line-backcolor)
(set-face-foreground 'hl-line line-forecolor)

;; Mark lines using output from git diff.
;; (use-package git-gutter
;;   :defer 0.5
;;   :config
;;   (global-git-gutter-mode +1)
;;   (git-gutter:linum-setup))

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (diff-hl-margin-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package fill-column-indicator
  :config
  (add-hook 'prog-mode-hook 'fci-mode)
  (setq fci-rule-column 80))

;;; ui.el ends here
