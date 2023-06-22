;; Configuration start
(setq inhibit-startup-message t)    ; Remove startup message
(setq visible-bell t)               ; Make notification visible instead of sound

(scroll-bar-mode -1)                ; Disable scrollbar
(tool-bar-mode -1)                  ; Disable toolbar
(tooltip-mode -1)                   ; Disable tooltip
(menu-bar-mode -1)                  ; Disable menubar
(set-fringe-mode '(10 . 10))        ; Set left and right margin in pixels

;; Enable line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
;; Disable line numners for some modes
(dolist (mode '(org-mode-hook
				term-mode-hook
				shell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; Load font
(set-face-attribute 'default nil :font "SauceCodePro Nerd Font" :height 120)

;; Set theme
(load-theme 'misterioso)

;; Basic editor config
(setq-default tab-width 4):

;; Install Straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; Prevent package.el loading packages prior to their init-file loading.
(setq package-enable-at-startup nil)

;; use-package integration with straight.el
(straight-use-package 'use-package)

;; Enable vertico
;; @see: https://github.com/minad/vertico
(use-package vertico
  :straight t
  ;; Fix this ------
  :bind (:map vertico-map
			  ("C-j" . vertico-next)
			  ("C-k" . vertico-previous)
			  ("C-q" . vertico-exit))
  :init
  (vertico-mode)

  ;; Different scroll margin
  ; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ; (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :straight t
  :init (savehist-mode))

;; Doom Modeline
(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1))

;; Rainbow delimiter
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Which key
(use-package which-key
  :straight t
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.1))

