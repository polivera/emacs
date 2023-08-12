;; ========================================================
;; Emacs configuration file === copy/pasta from scratch ===
;; ========================================================

;; Installing ELPACA package manager
;; ---------------------------------
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


;; Basic configurations
;; -----------------------------------------------
(setq inhibit-startup-message t)    ; Remove startup message
;; Make notification visible instead of sound
;; Don't do that on macos since it will show a horrible icon
(if (not (eq system-type 'darwin))
    (setq visible-bell t))               

;; In MacOS change option for command (to keep things the same)
(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta
          mac-option-modifier nil
          mac-control-modifier 'control
          mac-right-command-modifier 'super
          mac-right-control-modifier 'hyper))

;; Some basics
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

;; Create a tmp folder inside emacs config so all the backup files go there
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

;; Create another tmp directory for auto-saave files
(make-directory (expand-file-name "tmp/auto-saves" user-emacs-directory) t)

;; Set auto-saves to be store in the new folder
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/session" user-emacs-directory)
  auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))


;; SLIME Superior Lisp Interaction Mode for Emacs.
;; -----------------------------------------------
(use-package slime
  :demand t
  :config
  ;; Point inferior lisp program to common list implementation
  (setq inferior-lisp-program "sbcl"))


;; Evil Mode
;; -----------------------------------------------
(use-package evil
  :demand t
  :init
  (setq evil-want-keybinding nil)
  :config
  ;; Evil Startup
  (evil-mode)
  ;; Evil config
  (evil-set-undo-system 'undo-redo)
  ;; Evil keybindings
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop))
;; TODO disable evil mode on major mode 'slime-repl-mode
;; (maybe evil collections will fix that


;; Evil Collections
;; -----------------------------------------------
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))


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


;; Which Key
;; -----------------------------------------------
(use-package which-key
  :demand t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.2))


;; ---------------- Completion ---------------- ;;

;; Vertico
;; -----------------------------------------------
;; VERTical Iteractive Completion Framework
(use-package vertico
  :demand t
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

