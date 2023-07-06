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
                shell-mode-hook
                treemacs-mode-hook))
        (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Create a tmp folder inside emacs config so all the backup files go there
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

;; Create another tmp directory for auto-saave files
(make-directory (expand-file-name "tmp/auto-saves" user-emacs-directory) t)

;; Set auto-saves to be store in the new folder
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/session" user-emacs-directory)
  auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

;; If you want to disable lock files, uncomment this line (NOT RECOMMENDED)
;(setq create-lockfiles nil)

;; Adding package to take care of the rest
(use-package no-littering
  :straight t)

;; Load fonts
(set-face-attribute 'default nil :font "SauceCodePro Nerd Font" :height 120)
(set-face-attribute 'fixed-pitch nil :font "SauceCodePro Nerd Font" :height 120)
(set-face-attribute 'variable-pitch nil :font "Comfortaa" :height 120)

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

(use-package treemacs
  :straight t
  :defer t
  :init
    (with-eval-after-load 'winum
      (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil)

  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :straight t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :straight t)

;;(use-package treemacs-icons-dired
  ;;:after (all-the-icons)
  ;;:hook (dired-mode . treemacs-icons-dired-enable-once)
  ;;:straight t)

(use-package treemacs-magit
  :after (treemacs magit)
  :straight t)

(use-package lsp-treemacs
  :after lsp
  :straight t)

;;(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  ;;:after (treemacs persp-mode) ;;or perspective vs. persp-mode
  ;;:ensure t
  ;;:config (treemacs-set-scope-type 'Perspectives))

;;(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  ;;:after (treemacs)
  ;;:ensure t
  ;;:config (treemacs-set-scope-type 'Tabs))

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
	(setq projectile-project-search-path '(("~/Projects" . 4))))
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
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
          ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
          ("<tab>" . company-indent-or-complete-common))
  :config
  (company-keymap--unbind-quick-access company-active-map)
  (setq company-idle-delay 0.0
        company-minimum-prefix-length 1))

(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode)
  :config
  (setq lsp-ui-doc-position 'bottom))

(defun poli/eshell-config ()
  ;; Save command history
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-function 'eshell-truncate-buffer)
  ;; Bind some evil mode keys
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'eshell-list-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignore-dups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt
  :straight t)

(use-package eshell
  :straight t
  :hook (eshell-first-time-mode . poli/eshell-config)
  :config
  (eshell-git-prompt-use-theme 'powerline))

(defun poli/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  :hook (lsp-mode . poli/lsp-mode-setup))

(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode))

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

(use-package typescript-mode
  :straight t
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

;; Key Definition
(poli/leader-keys
  ;; Projectile shortcuts
  "p" '(projectile-command-map :which-key "Projectile")

  ;; Write to file
  "w" '(save-buffer :which-key "save buffer")

  ;; Treemacs
  "n" '(treemacs :which-key "open treemacs")

  ;; Find stuff
  "f" '(:ignore t :which-key "find")
  "ff" '(:ignore :which-key "files in project")
  "ffp" '(projectile-find-file :which-key "git files")
  "ffa" '(consult-file :which-key "all files")
  "fb" '(:ignore :which-key "buffers")
  "fbp" '(projectile-switch-to-buffer :which-key "project buffers")
  "fba" '(consult-buffer :which-key "project buffers")
  "fg" '(consult-ripgrep :which-key "all buffers")
  ;; Toggles
  "t"  '(:ignore t :which-key "toggles")
  "ts" '(hydra-text-scale/body :which-key "scale text")
  "tt" '(consult-theme :which-key "choose theme")
)
