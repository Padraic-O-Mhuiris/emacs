;;; init.el --- Padraic's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Patrick H Morris

;; Author: Patrick H Morris <patrick.morris.310@gmail.com>
;; Keywords: internal
;; URL: https://panadestein.github.io/emacsd/

;;; Commentary:
;; A fully fledged, reproducible Emacs configuration

;;; Code:

(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq file-name-handler-alist file-name-handler-alist-original)))

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(defvar pm/gc-cons-threshold 100000000)

(add-hook 'emacs-startup-hook ; hook run after loading init files
          (lambda ()
            (setq gc-cons-threshold pm/gc-cons-threshold
                  gc-cons-percentage 0.1
                  file-name-handler-alist file-name-handler-alist-original)))

(add-hook 'minibuffer-setup-hook (lambda ()
                                   (setq gc-cons-threshold (* pm/gc-cons-threshold 2))))
(add-hook 'minibuffer-exit-hook (lambda ()
                                  (garbage-collect)
                                  (setq gc-cons-threshold pm/gc-cons-threshold)))

(when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
  (progn
    (setq native-comp-async-report-warnings-errors nil)
    (setq native-comp-deferred-compilation t)
    (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
    (setq package-native-compile t)))

(defvar pm/initialized nil)

(add-hook 'emacs-startup-hook
  	(lambda ()
  	  (setq pm/initialized t)))

(setq user-full-name "Patrick H Morris"
      user-mail-address "patrick.morris.310@gmail.com")

(setq inhibit-default-init t)

(let ((customization-file
       (expand-file-name "custom.el" user-emacs-directory)))
  (unless (file-exists-p customization-file)
    (write-region "" nil customization-file))
  (setq custom-file customization-file)
  (load custom-file 'noerror))

(setq inhibit-startup-message t)

;; (setq inhibit-startup-echo-area-message "Hello!!!")
(defun display-startup-echo-area-message ()
  (display-startup-time))

(setq initial-buffer-choice "~/.config/emacs/config.org")
;; (add-hook 'emacs-startup-hook
;; 	  (lambda ()
;; 	    (split-window-right) ; Split the window vertically
;; 	    (other-window 1)          ; Move to the new split pane
;; 	    (switch-to-buffer "*Messages*")))

(setq initial-scratch-message nil)

(setq large-file-warning-threshold 100000000)

(global-auto-revert-mode t)
(setq auto-revert-interval 1)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(require 'so-long)

(add-hook 'after-init-hook 'global-so-long-mode)

(setq make-backup-files nil)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)

(setq visible-bell t)

(setq ring-bell-function 'ignore)

(blink-cursor-mode -1)

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(fset 'yes-or-no-p 'y-or-n-p)

(if init-file-debug
     (setq warning-minimum-level :debug)
   (setq warning-minimum-level :emergency))

(require 'savehist)
(savehist-mode)

(require 'doom-themes)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

(unless pm/initialized (load-theme 'doom-moonlight t)) ; only call on initialization
(doom-themes-visual-bell-config)
(doom-themes-org-config)

(set-face-attribute 'default nil :font "Iosevka NFM" :height 100)

(require 'all-the-icons)

(require 'doom-modeline)
(doom-modeline-mode 1)
(setq doom-modeline-height 55)
(setq doom-modeline-buffer-file-name-style 'relative-to-project)
(setq doom-line-numbers-style 'relative)
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-buffer-state-icon t)
(setq doom-modeline-major-mode-color-icon t)

(set-fringe-mode 10)

(require 'general)
(general-evil-setup t)

(general-create-definer pm/leader
  :keymaps '(normal insert visual emacs)
  :prefix "SPC"
  :global-prefix "C-SPC")

(require 'which-key)
(which-key-mode)
(setq which-key-idle-delay 0)

(pm/leader
  "r" '(pm/reload-config :which-key "Reload config")
  "u" '(:ignore t :which-key "ui")
  "ut" '(counsel-load-theme :which-key "Select Theme")
  "m" '(:ignore t :which-key "magit")
  "mm" '(magit-status-here :which-key "status"))

(general-define-key
 "<escape>" 'keyboard-escape-quit)

(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(setq evil-want-C-u-scroll t)
(setq evil-want-C-i-jump nil)
(setq evil-undo-system 'undo-tree)

(require 'evil)
(evil-mode 1)

(require 'evil-collection)
(evil-collection-init)

(evil-set-initial-state 'messages-buffer-mode 'normal)
(evil-set-initial-state 'dashboard-mode 'normal)

(general-def 'evil-insert-state-map
  "C-g" 'evil-normal-state
  "C-h" 'evil-delete-backward-char-and-join)

(general-define-key 
 :states 'motion
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line)

(require 'undo-tree)
(general-define-key
 "C-x u" 'undo-tree-visualize)

(global-undo-tree-mode)
(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-visualizer-diff t)

(require 'magit)
(setopt magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1) ; What does this do?

(require 'org)
(require 'org-bullets)

;; Replace the content marker, “⋯”, with a nice unicode arrow.
(setq org-ellipsis " ⤵")
;; Avoid accidentally editing folded regions, say by adding text after an Org “⋯”.
(setq org-catch-invisible-edits t)
;; Tab should do indent in code blocks
(setq org-src-tab-acts-natively t)
;; Give quote and verse blocks a nice look.
(setq org-fontify-quote-and-verse-blocks t)
(setq org-hide-emphasis-markers t)
(setq org-startup-indented t)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist
                       '("el" . "src emacs-lisp"))

(add-hook 'org-mode-hook #'(lambda ()

                           ;; make the lines in the buffer wrap around the edges of the screen.

                           ;; to press C-c q  or fill-paragraph ever again!
                           (visual-line-mode)
                           (org-indent-mode)))

;; (use-package projectile  
;;   :straight t
;;   :diminish projectile-mode
;;   :config (projectile-mode)
;;   :custom ((projectile-completion-system 'ivy))
;;   :bind-keymap
;;   ("C-c p" . projectile-command-map)
;;   :init
;;   ;; NOTE: Set this to the folder where you keep your Git repos!
;;   (when (file-directory-p "~/code")
;;     (setq projectile-project-search-path '("~/code")))
;;   (setq projectile-switch-project-action #'projectile-dired))

;; (use-package counsel-projectile  
;;   :straight t
;;   :after projectile
;;   :config (counsel-projectile-mode))

(require 'vertico)
(vertico-mode)

;; Different scroll margin
(setq vertico-scroll-margin 0)

;; Show more candidates
(setq vertico-count 20)

;; Grow and shrink the Vertico minibuffer
(setq vertico-resize t)

;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
(setq vertico-cycle t)

(require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))

(require 'corfu)

(global-corfu-mode)
(setq corfu-auto t
      corfu-quit-no-match 'separator)

(setq completion-cycle-threshold 3)
(setq tab-always-indent 'complete)

(require 'marginalia)
(marginalia-mode)

(require 'consult)

(general-define-key
 ;; C-c bindings in `mode-specific-map'
 "C-c M-x" 'consult-mode-command
 "C-c h" 'consult-history
 "C-c k" 'consult-kmacro
 "C-c m" 'consult-man
 "C-c i" 'consult-info
 [remap Info-search] 'consult-info
 ;; C-x bindings in `ctl-x-map'
 "C-x M-:" 'consult-complex-command
 "C-x b" 'consult-buffer
 "C-x 4 b" 'consult-buffer-other-window
 "C-x 5 b" 'consult-buffer-other-frame
 "C-x r b" 'consult-bookmark
 "C-x p b" 'consult-project-buffer
 ;; Custom M-# bindings for fast register access
 "M-#" 'consult-register-load
 "M-'" 'consult-register-store
 "C-M-#" 'consult-register
 ;; Other custom bindings
 "M-y" 'consult-yank-pop
 ;; M-g bindings in `goto-map'
 "M-g e" 'consult-compile-error
 "M-g f" 'consult-flymake
 "M-g g" 'consult-goto-line
 "M-g M-g" 'consult-goto-line
 "M-g o" 'consult-outline
 "M-g m" 'consult-mark
 "M-g k" 'consult-global-mark
 "M-g i" 'consult-imenu
 "M-g I" 'consult-imenu-multi
 ;; M-s bindings in `search-map'
 "M-s d" 'consult-find
 "M-s D" 'consult-locate
 "M-s g" 'consult-grep
 "M-s G" 'consult-git-grep
 "M-s r" 'consult-ripgrep
 "M-s l" 'consult-line
 "M-s L" 'consult-line-multi
 "M-s k" 'consult-keep-lines
 "M-s u" 'consult-focus-lines
 ;; Isearch integration
 "M-s e" 'consult-isearch-history)

(general-define-key
 :keymaps 'isearch-mode-map
 "M-e" 'consult-isearch-history
 "M-s e" 'consult-isearch-history
 "M-s l" 'consult-line
 "M-s L" 'consult-line-multi)

(general-define-key
 :keymaps 'minibuffer-local-map
 "M-s" 'consult-history
 "M-r" 'consult-history)

;;   ;; Enable automatic preview at point in the *Completions* buffer. This is
;;   ;; relevant when you use the default completion UI.
;;   :hook (completion-list-mode . consult-preview-at-point-mode)

;;   ;; The :init configuration is always executed (Not lazy)
;;   :init

;;   ;; Optionally configure the register formatting. This improves the register
;;   ;; preview for `consult-register', `consult-register-load',
;;   ;; `consult-register-store' and the Emacs built-ins.
;;   (setq register-preview-delay 0.5
;;         register-preview-function #'consult-register-format)

;;   ;; Optionally tweak the register preview window.
;;   ;; This adds thin lines, sorting and hides the mode line of the window.
;;   (advice-add #'register-preview :override #'consult-register-window)

;;   ;; Use Consult to select xref locations with preview
;;   (setq xref-show-xrefs-function #'consult-xref
;;         xref-show-definitions-function #'consult-xref)

;;   ;; Configure other variables and modes in the :config section,
;;   ;; after lazily loading the package.
;;   :config

;;   ;; Optionally configure preview. The default value
;;   ;; is 'any, such that any key triggers the preview.
;;   ;; (setq consult-preview-key 'any)
;;   ;; (setq consult-preview-key "M-.")
;;   ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
;;   ;; For some commands and buffer sources it is useful to configure the
;;   ;; :preview-key on a per-command basis using the `consult-customize' macro.
;;   (consult-customize
;;    consult-theme :preview-key '(:debounce 0.2 any)
;;    consult-ripgrep consult-git-grep consult-grep
;;    consult-bookmark consult-recent-file consult-xref
;;    consult--source-bookmark consult--source-file-register
;;    consult--source-recent-file consult--source-project-recent-file
;;    ;; :preview-key "M-."
;;    :preview-key '(:debounce 0.4 any))

;;   ;; Optionally configure the narrowing key.
;;   ;; Both < and C-+ work reasonably well.
;;   (setq consult-narrow-key "<") ;; "C-+"

;;   ;; Optionally make narrowing help available in the minibuffer.
;;   ;; You may want to use `embark-prefix-help-command' or which-key instead.
;;   ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

;;   ;; By default `consult-project-function' uses `project-root' from project.el.
;;   ;; Optionally configure a different project root function.
;;   ;;;; 1. project.el (the default)
;;   ;; (setq consult-project-function #'consult--default-project--function)
;;   ;;;; 2. vc.el (vc-root-dir)
;;   ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
;;   ;;;; 3. locate-dominating-file
;;   ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
;;   ;;;; 4. projectile.el (projectile-project-root)
;;   ;; (autoload 'projectile-project-root "projectile")
;;   ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
;;   ;;;; 5. No project support
;;   ;; (setq consult-project-function nil)
;; )

(require 'embark)
(require 'embark-consult)

(general-define-key
 "C-." 'embark-act
 "C-;" 'embark-dwim
 "C-h B" 'embark-bindings)

;; Optionally replace the key help with a completing-read interface
(setq prefix-help-command #'embark-prefix-help-command)
;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
;; strategy, if you want to see the documentation from multiple providers.
(add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)


(add-to-list 'display-buffer-alist
             '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
               nil
               (window-parameters (mode-line-format . none))))

(add-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode)

(require 'highlight-numbers)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers 'relative)
;; (dolist (mode '(org-mode-hook 
;;   	      term-mode-hook))
;;   (add-hook mode (lambda () (display-line-numbers-mode 0))))

(require 'smartparens)
(add-hook 'prog-mode-hook 'smartparens-mode)

(require 'esup)

(defun pm/reload-config ()
  "Reloads the emacs configuration"
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))

(defun display-startup-time ()
  (message "Emacs ready in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook 'display-startup-time)

(provide 'init.el)
;;; init.el ends here
