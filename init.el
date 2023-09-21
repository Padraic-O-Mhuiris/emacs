;; #############################################################################
;; General Configuration
;; #############################################################################

;; Remove uneeded UI elements
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(setq visible-bell t)

;; Font
(set-face-attribute 'default nil :font "Iosevka NFM" :height 120)

;; Theme
(load-theme 'wombat)

;; User configuration
(setq user-full-name "Patrick H Morris"
      user-mail-address "patrick.morris.310@gmail.com")

;; use-package
(setq use-package-always-ensure t)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; #############################################################################
;; general.el
;; #############################################################################
(use-package general)

;; global keybindings

;; - Make ESC quit prompts
;; (general-define-key
;;  "<escape>" 'keyboard-escape-quit
;;  "C-c t" 'clm/toggle-command-log-buffer)

;; (general-create-definer leader-key
;;   ;; :prefix my-leader
;;   :prefix "SPC")

;; (general-create-definer leader-group
;;   :prefix "SPC m")

;; ;; Windows
;; (general-create-definer window-leader
;;   :prefix "SPC w")

;; (window-leader
;;  :keymaps 'normal
;;  "d" '((lambda ()
;; 	 (interactive)
;; 	 (delete-window)
;; 	 (balance-windows)))
;;  "v" '((lambda ()
;; 	 (interactive)
;; 	 (split-window-vertically)
;; 	 (balance-windows)))
;;  "h" '((lambda ()
;; 	 (interactive)
;; 	 (split-window-horizontally)
;; 	 (balance-windows))))
;; Random keybindings

;; #############################################################################
;; doom-modeline
;; #############################################################################
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; #############################################################################
;; Ivy
;; #############################################################################
(use-package ivy
  :diminish
  :config
  (ivy-mode 1))

;; #############################################################################
;; command-log-mode
;; #############################################################################
(use-package command-log-mode
  :diminish
  :config (global-command-log-mode)
  :bind (("C-c t" . clm/toggle-command-log-buffer)))

;; #############################################################################
;; Evil-mode
;; #############################################################################
(use-package evil
  :diminish
  :init
  (evil-mode 1))

;; #############################################################################
;; which-key
;; #############################################################################
(use-package which-key
  :diminish
  :init (which-key-mode))
