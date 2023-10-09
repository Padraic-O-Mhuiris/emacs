(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(setq visible-bell t)

(setq user-full-name "Patrick H Morris"
      user-mail-address "patrick.morris.310@gmail.com")

(setq initial-buffer-choice "~/.config/emacs/config.org")
;; (add-hook 'emacs-startup-hook
;; 	  (lambda ()
;; 	    (split-window-right) ; Split the window vertically
;; 	    (other-window 1)          ; Move to the new split pane
;; 	    (switch-to-buffer "*Messages*")))

(global-auto-revert-mode t)

(defvar pm/initialized nil)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq pm/initialized t)))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq make-backup-files nil)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(set-face-attribute 'default nil :font "Iosevka NFM" :height 100)

(use-package doom-themes  
  :straight t
  :after general
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (unless pm/initialized (load-theme 'doom-moonlight t)) ; only call on initialization
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package doom-modeline  
  :straight t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15))
  :config
  (use-package all-the-icons
    :straight t))

(column-number-mode)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook 
			  term-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package rainbow-delimiters  
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package org
  :straight t
  :config
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
  (use-package org-bullets
    :straight
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))))

(use-package org-tempo
  :ensure nil 
  :after org
  :config
  (add-to-list 'org-structure-template-alist
		 '("el" . "src emacs-lisp")))

(use-package general
  :straight t
  :config
  (general-evil-setup t)

  (general-define-key
   "<escape>" 'keyboard-escape-quit
   "C-M-j" 'counsel-switch-buffer)

  (general-create-definer pm/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (pm/leader-key-def
    "r" '(pm/reload-config :which-key "Reload config")
    "u" '(:ignore t :which-key "ui")
    "ut" '(counsel-load-theme :which-key "magit")
    "m" '(:ignore t :which-key "magit")
    "mm" '(magit-status-here :which-key "status")))

(use-package evil  
    :straight t
    :after undo-tree
    :init
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
    (setq evil-want-C-u-scroll t)
    (setq evil-want-C-i-jump nil)
    (setq evil-undo-system 'undo-tree)
    :config
    (evil-mode 1)
    (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
    (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

    (evil-global-set-key 'motion "j" 'evil-next-visual-line)
    (evil-global-set-key 'motion "k" 'evil-previous-visual-line)


    (evil-set-initial-state 'messages-buffer-mode 'normal)
    (evil-set-initial-state 'dashboard-mode 'normal))

  (use-package evil-collection  
    :straight t
    :after evil
    :config
    (evil-collection-init))

(use-package magit  
  :straight t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package projectile  
  :straight t
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/code")
    (setq projectile-project-search-path '("~/code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile  
  :straight t
  :after projectile
  :config (counsel-projectile-mode))

(use-package undo-tree
  :straight t
  :diminish                       ;; Don't show an icon in the modeline
  :bind ("C-x u" . undo-tree-visualize)
  :hook (org-mode . undo-tree-mode) ;; For some reason, I need this. FIXME.
  :config
    ;; Always have it on
    (global-undo-tree-mode)

    ;; Each node in the undo tree should have a timestamp.
    (setq undo-tree-visualizer-timestamps t)

    ;; Show a diff window displaying changes between undo nodes.
    (setq undo-tree-visualizer-diff t))

(use-package which-key  
  :straight t
  :diminish which-key-mode
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0))

(defun pm/reload-config ()
  "Reloads the emacs configuration"
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))

(use-package embark
  :straight t
  :ensure t
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
  :straight t
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package vertico
  :straight t
  :init
  (vertico-mode)

  ;; Different scroll margin
  (setq vertico-scroll-margin 0)

  ;; Show more candidates
  (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t))

(use-package savehist
  :straight t
  :init
  (savehist-mode))

(use-package marginalia
  :straight t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package consult
  :straight t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
)

;; #############################################################################
;; hydra
;; #############################################################################
(use-package hydra
  :straight t)

;; (defhydra hydra-text-scale (:timeout 4)
;;  "scale text"
;;  ("+" text-scale-increase "+")
;;  ("-" text-scale-decrease "-")
;;  ("f" nil "finished" :exit t))

;; (pm/leader-key-def
;;  "ts" '(hydra-text-scale/body :which-key "scale text"))


;; #############################################################################
;; ivy
;; #############################################################################
;; (use-package ivy  
;;   :straight t
;;   :after counsel
;;   :diminish
;;   :config
;;   (ivy-mode 1)

;;   (use-package ivy-rich    
;;     :straight t
;;     :init (ivy-rich-mode 1)))

;; #############################################################################
;; counsel
;; #############################################################################
;; (use-package counsel
;;   :straight t
;;   :bind (("M-x" . counsel-M-x)
;; 	 ("C-x b" . counsel-ibuffer)
;; 	 :map minibuffer-local-map
;; 	 ("C-r" . 'counsel-minibuffer-history))
;;   :config
;;   (setq ivy-initial-inputs-alist nil)) 


;; #############################################################################
;; command-log-mode
;; #############################################################################
(use-package command-log-mode  
  :straight t
  :diminish
  :config (global-command-log-mode)
  :bind (("C-c t" . clm/toggle-command-log-buffer)))

;; #############################################################################
;; which-key
;; #############################################################################

;; #############################################################################
;; rainbow-delimiters
;; #############################################################################

;; #############################################################################
;; helpful
;; #############################################################################


;; #############################################################################
;; projectile
;; #############################################################################

;; #############################################################################
;; magit
;; #############################################################################

;; #############################################################################
;; org
;; #############################################################################
