;; Configuration start
(setq inhibit-startup-message t)    ; Remove startup message
;; Also here you can do 'system-type' so we don't show visible bell on macos
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
(dolist (mode '(
				;;org-mode-hook
				term-mode-hook
				dired-mode-hook
				shell-mode-hook))
		(add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Load fonts
(set-face-attribute 'default nil :font "SauceCodePro Nerd Font" :height 120)
(set-face-attribute 'fixed-pitch nil :font "SauceCodePro Nerd Font" :height 120)
(set-face-attribute 'variable-pitch nil :font "Comfortaa" :height 120)

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

;; Vertico package
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
  (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ; (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :straight t
  :init (savehist-mode))

;; Consult package
(use-package consult
  :straight t
  :hook (completion-list-mode . consult-preview-at-point-mode))

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
  (setq which-key-idle-delay 0.2))

;; Insatll Catppuccin theme
(use-package catppuccin-theme
  :straight t
  :config
  (setq catppuccin-flavor 'mocha)
  :init
  (load-theme 'catppuccin t))

;; Icons
(use-package all-the-icons
  :straight t
  :if (display-graphic-p))
(use-package nerd-icons
  :straight t)

(use-package general
  :straight t
  :config
  (general-create-definer poli/leader-keys
						  :keymaps '(normal insert visual emacs)
						  ;; This will be used as a leader in all modes but insert
						  :prefix "SPC"
						  ;; This will be used as a leader key when on insert mode
						  :global-prefix "C-SPC"))

(use-package evil
  :straight t
  :init
  ;; TODO: Learn what this does
  (setq evil-want-integration t)
  ;; Integration with other modes deactivate becase another package is used for that
  (setq evil-want-keybinding nil)
  ;; Make it so C-u scroll instead of emacs default behaviour
  (setq evil-want-C-u-scroll t)
  (evil-mode 1)
  :config
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

(use-package evil-collection
  :straight t
  :after evil
  :init
  (evil-collection-init))

;; Hydra package
(use-package hydra
  :straight t)

;; Hydra definitions
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("r" (text-scale-set 0) "reset")
  ("q" nil "finish" :exit t))

;; Projectile
(use-package projectile
  :straight t
  :init
  (projectile-mode)
  :config
  (when (file-directory-p "~/Projects")
	;; Limit the amount of subdirectories on which projectile will look into
	(setq projectile-project-search-path '(("~/Projects" . 1))))
  ;; This will show the directory structure when you switch project
  (setq projectile-switch-project-action #'projectile-dired))

;; Magit
(use-package magit
  :straight t
  :custom
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

;; Forge
(use-package forge
  :straight t
  :after magit)

;; Org Mode
(defun poli/org-mode-setup ()
  (variable-pitch-mode 1))

(use-package org
  :straight t
  :hook (org-mode . poli/org-mode-setup)
  :config
  ;; Face exceptions: These are the modes that will keep the fixed-pitch font
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)


  ;; Structure templates for code snippets, used on org-babel
  ;; org-tempo is required for the templates to work
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

  )

(defun poli/org-auto-tangle ()
(when (string-equal (buffer-file-name)
					(expand-file-name "~/Projects/Personal/emacs/Config.org"))
(let ((org-confirm-babel-evaluate nil))
(org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'poli/org-auto-tangle)))

  ;(use-package org-bullets
  ;  :straight t
  ;  :after org
  ;  :hook (org-mode . org-bullets-mode))

;; Key Definition
(poli/leader-keys
  ;; Projectile shortcuts
  "p" '(projectile-command-map :which-key "Projectile")
  ;; Write to file
  "w" '(save-buffer :which-key "save buffer")
  ;; Find stuff
  "f" '(:ignore t :which-key "Find")
  "ff" '(consult-find :which-key "Files")
  "fb" '(consult-buffer :which-key "Buffer")
  "fg" '(consult-grep :which-key "Grep")
  ;; Toggles
  "t"  '(:ignore t :which-key "toggles")
  "ts" '(hydra-text-scale/body :which-key "scale text")
  "tt" '(consult-theme :which-key "choose theme")
)
