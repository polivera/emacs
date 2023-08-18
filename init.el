;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

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

(defvar poli/tmp_folder (expand-file-name "tmp" user-emacs-directory))

(setq inhibit-startup-message t)    ; Remove startup message

;; In MacOS change option for command (to keep things the same)
(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta
          mac-option-modifier nil
          mac-control-modifier 'control
          mac-right-command-modifier 'super
          mac-right-control-modifier 'control))

; Remove startup message
(setq inhibit-startup-message t)

; Disable sound bell (except on mac, visual is horrible)
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

; Create another tmp directory for auto-saave files
(make-directory poli/tmp_folder t)

; Create a tmp folder inside emacs config so all the backup files go there
(setq backup-directory-alist `(("." . ,(expand-file-name "backups/" poli/tmp_folder))))

; Set auto-saves to be store in the new folder
(setq auto-save-list-file-prefix (expand-file-name "auto-saves/session" poli/tmp_folder)
    auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-saves/" poli/tmp_folder) t)))

; Load fonts depending on the OS
(cond
  ((eq system-type 'darwin) ;; MacOS
    (set-face-attribute 'default nil :font "Iosevka Term" :height 145)
    (set-face-attribute 'fixed-pitch nil :font "Iosevka Term" :height 145)
    (set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height 145))
  (t ;; Other OS (I use linux btw)
    (set-face-attribute 'default nil :font "Iosevka Term" :height 110)
    (set-face-attribute 'fixed-pitch nil :font "Iosevka Term" :height 120)
    (set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height 140)))

;; EF Themes
(use-package ef-themes
  :demand t)

;; Doom themes
(use-package doom-themes
  :demand t
  :config
  (load-theme 'doom-palenight t))

; Doom Modeline
(use-package doom-modeline
 :demand t
 :config
 (defvar doom-modeline-height 40)
 (doom-modeline-mode 1))

; Evil Mode
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

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode)) ;; globally enable evil-commentary

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1)
  (add-hook 'emacs-lisp-mode-hook (lambda ()
                                (push '(?` . ("`" . "'")) evil-surround-pairs-alist))))

(use-package general
  :demand t
  :after evil
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

(use-package which-key
  :demand t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.2))

; VERTical Iteractive Completion Framework
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

(use-package consult
  :demand t
  :config
  (setq consult-project-root-function #'projectile-project-root))

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

(defun poli/org-setup()
  ;; Should I remove variable pitch font from org mode?
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)

  (local-set-key (kbd "C-<space>") 'tempo-complete-tag)
  (require 'org-tempo)

  ; Create a list of templates
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

  ; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)

  ; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.15)
                  (org-level-4 . 1.15)
                  (org-level-5 . 1.125)
                  (org-level-6 . 1.125)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Iosevka Aile" :weight 'regular :height (cdr face)))
)

(use-package org
    :demand t
    :hook (org-mode . poli/org-setup))

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

; Rainbow delimiter
(use-package rainbow-delimiters
  :demand t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Adding treesitter github sources list
(setq treesit-language-source-alist
'((bash "https://github.com/tree-sitter/tree-sitter-bash")
  (c "https://github.com/tree-sitter/tree-sitter-c")
  (cmake "https://github.com/uyha/tree-sitter-cmake")
  (common-lisp "https://github.com/theHamsta/tree-sitter-commonlisp")
  (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
  (css "https://github.com/tree-sitter/tree-sitter-css")
  (csharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
  (elisp "https://github.com/Wilfred/tree-sitter-elisp")
  (go "https://github.com/tree-sitter/tree-sitter-go")
  (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
  (html "https://github.com/tree-sitter/tree-sitter-html")
  (js . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
  (json "https://github.com/tree-sitter/tree-sitter-json")
  (lua "https://github.com/Azganoth/tree-sitter-lua")
  (make "https://github.com/alemuller/tree-sitter-make")
  (markdown "https://github.com/ikatyang/tree-sitter-markdown")
  (python "https://github.com/tree-sitter/tree-sitter-python")
  (r "https://github.com/r-lib/tree-sitter-r")
  (rust "https://github.com/tree-sitter/tree-sitter-rust")
  (toml "https://github.com/tree-sitter/tree-sitter-toml")
  (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
  (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
  (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; (setq treesit-load-name-override-list
;;   '((go-mod "libtree-sitter-go-mod" "tree_sitter_gomod"))

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
      ("<f6>" . gofmt))
:config
(require 'lsp-go)
;; Set Gopls tags
(setq lsp-go-env '((GOFLAGS . "-tags=unit,integration")))
(setq lsp-go-analyses
  '((field-alignment . t)
    (nillness . t)))
;; Gopath
(add-to-list 'exec-path "~/.local/share/go/bin"))

(elpaca-wait)

;; Consult General keybindings
(poli/leader-keys
  "bb" '(consult-buffer :wk "consult buffer")
  "Bb" '(consult-bookmark :wk "consult bookmark")
  "ht" '(consult-theme :wk "consult theme")
  "sr" '(consult-ripgrep :wk "consult rg")
  "sg" '(consult-grep :wk "consult grep")
  "sG" '(consult-git-grep :wk "consult git grep")
  "sf" '(consult-find :wk "consult find")
  "sF" '(consult-locate :wk "consult locate")
  "sl" '(consult-line :wk "consult line")
  "sy" '(consult-yank-from-kill-ring :wk "consult yank from kill ring")
  "i" '(consult-imenu :wk "consult imenu"))

;; SLIME Superior Lisp Interaction Mode for Emacs.
;; -----------------------------------------------
(use-package slime
  :demand t
  :config
  ;; Point inferior lisp program to common list implementation
  (setq inferior-lisp-program "sbcl"))
