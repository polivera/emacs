;; Installing ELPACA Package Manager
(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed
(elpaca-wait)

(defun poli/org-setup()
      ;; Should I remove variable pitch font from org mode?
      (variable-pitch-mode 0)
      (local-set-key (kbd "C-<space>") 'tempo-complete-tag)
      (require 'org-tempo)
      (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
)

(use-package org
    :demand t
    :hook (org-mode . poli/org-setup))

(setq inhibit-startup-message t)    ; Remove startup message

;; In MacOS change option for command (to keep things the same)
(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta
          mac-option-modifier nil
          mac-control-modifier 'control
          mac-right-command-modifier 'super
          mac-right-control-modifier 'control))

(if (not (eq system-type 'darwin))
    (setq visible-bell t))

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

;; Tab configuration
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; (savehist-mode 1)
(use-package savehist
  :elpaca nil
  :init
  (savehist-mode))

;; Create a tmp folder inside emacs config so all the backup files go there
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

;; Create another tmp directory for auto-saave files
(make-directory (expand-file-name "tmp/auto-saves" user-emacs-directory) t)

;; Set auto-saves to be store in the new folder
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/session" user-emacs-directory)
    auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

;; Load fonts
;; -----------------------------------------------
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 105)
(set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font" :height 105)

;; EF Themes
;; -----------------------------------------------
(use-package ef-themes
    :demand t
    :config
    (load-theme 'ef-maris-dark t))

;; Doom Modeline
;; -----------------------------------------------
(use-package doom-modeline
    :demand t
    :config
    (doom-modeline-mode 1))

;; Evil Mode
;; -----------------------------------------------
(use-package evil
    :demand t
    :init
    (setq evil-want-keybinding nil)
    (setq evil-want-C-u-scroll t)
    :config
    ;; Evil Startup
    (evil-mode)
    ;; Evil config
    (evil-set-undo-system 'undo-redo)
    ;; Evil keybindings
    (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
    ;(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
    (evil-set-initial-state 'slime-repl-mode 'emacs))


;; Evil Collections
;; -----------------------------------------------
(use-package evil-collection
    :after evil
    :config
    (evil-collection-init))

(use-package general
  :demand t
  :config
  (general-evil-setup)
  ;; integrate general with evil

  ;; set up 'SPC' as the global leader key
  (general-create-definer poli/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode
)

;; Vertico
;; -----------------------------------------------
;; VERTical Iteractive Completion Framework
(use-package vertico
    :demand t
    :bind (:map vertico-map
            ("C-j" . vertico-next)
            ("C-k" . vertico-previous)
            ("C-q" . vertico-exit))
    :config
    (vertico-mode 1))

;; Marginalia
;; Nice description on the completion framework
;; entries
;; -----------------------------------------------
(use-package marginalia
    :after vertico
    :config
    (marginalia-mode 1))

;; Orderless
;; Better matching for vertico
;; -----------------------------------------------
(use-package orderless
    :after vertico
    :custom
    (completion-styles '(orderless basic))
    (completion-category-overrides '((file (styles basic partial-completion)))))

;; Corfu
;; Auto completion example
(use-package corfu
  :demand t
  :custom
  (corfu-auto t)          ;; Enable auto completion
  (corfu-auto-delay 0.0)
  ;; (corfu-separator ?_) ;; Set to orderless separator, if not using space
  :bind
  ;; Another key binding can be used, such as S-SPC.
  (:map corfu-map ("M-SPC" . corfu-insert-separator))
  :init
  (global-corfu-mode))

;; Projectile
(use-package projectile
    :demand t
    :init
    (projectile-mode)
    :config
    (when (file-directory-p "~/Projects")
    ;; Limit the amount of subdirectories on which projectile will look into
    (setq projectile-project-search-path '(("~/Projects" . 4))))
    ;; This will open a new project in Dired
    (setq projectile-switch-project-action #'projectile-dired))

;; Magit
(use-package magit
  :demand t
  :custom
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

;; LSP Configuration
(use-package lsp-mode
  :demand t
  :after corfu
  :custom
  (lsp-completion-provider :none)
  :init
  (setq lsp-keymap-prefix "C-c l")
  (defun poli/lsp-setup ()
      (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
      (lsp-headerline-breadcrumb-mode))
  (defun poli/lsp-completion-setup()
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
            '(orderless))) ;; Configure orderless
  :commands
  (lsp lsp-deferred)
  :config
  (lsp-enable-which-key-integration t)
  :hook
  (lsp-mode . poli/lsp-setup)
  (lsp-completion-mode . poli/lsp-completion-setup))

(use-package lsp-ui
  :demand t
  :after lsp-mode
  :commands lsp-ui-mode)

(use-package flycheck
  :demand t
  :init (global-flycheck-mode))

;; Golang configuration
(use-package go-mode
:demand t
:hook (
  (go-mode . lsp-deferred)
)
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

;; SLIME Superior Lisp Interaction Mode for Emacs.
;; -----------------------------------------------
(use-package slime
  :demand t
  :config
  ;; Point inferior lisp program to common list implementation
  (setq inferior-lisp-program "sbcl"))












;; Which Key
;; -----------------------------------------------
(use-package which-key
  :demand t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.2))

;; ---------------- Completion ---------------- ;;
