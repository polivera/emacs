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
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :config
  (setq consult-project-root-function #'projectile-project-root)
)

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
:straight t
;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
;; available in the *Completions* buffer, add it to the
;; `completion-list-mode-map'.
:bind (:map minibuffer-local-map
       ("M-A" . marginalia-cycle))
;; The :init section is always executed.
:init

;; Marginalia must be actived in the :init section of use-package such that
;; the mode gets enabled right away. Note that this forces loading the
;; package.
(marginalia-mode))

(use-package embark
  :straight t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :straight t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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

  (evil-set-undo-system 'undo-redo)

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
	(setq projectile-project-search-path '(("~/Projects" . 3))))
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
  (variable-pitch-mode 0))

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

(use-package org-bullets
  :straight t
  :after org
  :hook (org-mode . org-bullets-mode))

(use-package flycheck
  :straight t
  :init (global-flycheck-mode))

(use-package company
  :straight t
  :hook ((emacs-lisp-mode . (lambda ()
                              (setq-local company-backends '(company-elisp))))
         (emacs-list-mode . company-mode))
  :config
  (company-keymap--unbind-quick-access company-active-map)
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1))

(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package typescript-mode
  :straight t
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

;; Golang configuration
(use-package go-mode
:straight t
:hook ((go-mode . lsp-deferred)
       (go-mode . company-mode))
:bind (:map go-mode-map
        ("<f6>" . gofmt)
        ("C-c 6" . gofmt))
:config
(require 'lsp-go)
(setq lsp-go-analyses
  '((field-alignment . t)
    (nillness . t)))
;; Gopath
(add-to-list 'exec-path "~/.local/share/go/bin"))

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
  "fg" '(consult-ripgrep :which-key "Grep")
  ;; Toggles
  "t"  '(:ignore t :which-key "toggles")
  "ts" '(hydra-text-scale/body :which-key "scale text")
  "tt" '(consult-theme :which-key "choose theme")
)
