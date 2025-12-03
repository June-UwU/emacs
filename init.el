;; -------------------------------
;; Basic Emacs Configuration
;; -------------------------------
(setq make-backup-files nil) ; stop creating ~ files(setq package-check-signature nil)
(setq inhibit-startup-message t)
(if (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'tooltip-mode)
    (tooltip-mode -1))
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
(if (fboundp 'set-fringe-mode)
    (set-fringe-mode -1))

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Set default font and theme (optional)
(set-face-attribute 'default nil :height 120)

;; Make cursor a thin vertical bar
(setq-default cursor-type 'bar)
(blink-cursor-mode 0)

;; Make backspace behave normally
(normal-erase-is-backspace-mode 0)
(define-key key-translation-map [?\C-h] [?\C-?]) ;; fix Ctrl-h in terminal

;; Ensure DEL deletes backwards
(global-set-key (kbd "DEL") 'backward-delete-char-untabify)

;; Enable line numbers globally
(global-display-line-numbers-mode t)

;; -------------------------------
;; Package Management with use-package
;; -------------------------------

(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")))

(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; -------------------------------
;; Essential Packages
;; -------------------------------

;; Enable Vertico for minimal completion UI
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; which-key: shows available keybindings
(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.5))

;; magit: git integration
(use-package magit
  :bind ("C-x g" . magit-status))

;; projectile: project management
(use-package projectile
  :init
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map))
  ;;:config
  ;;(setq projectile-project-search-path '("~/projects/")))

(use-package ripgrep
  :ensure t)

(use-package projectile-ripgrep
  :ensure t
  :after projectile
  :bind (:map projectile-command-map
              ("s r" . projectile-ripgrep)))
(use-package xcscope
  :ensure t
  :config
  (cscope-setup))

(use-package consult
  :ensure t
  :bind (("C-c f" . consult-ripgrep)))

;; bind-key: cleaner keybinding declarations
(use-package bind-key)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-tokyo-nightev") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config))

(use-package nerd-icons
  :ensure t
  :config
  (set-frame-font "Fira Code Retina" nil t)
  (setq nerd-icons-font-family "Fira Code Retina")
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package lsp-mode
  :ensure t
  :hook ((c-mode . lsp)
	 (c++-mode . lsp)
	 (pythom-mode . lsp))
  :commands lsp)

(use-package neotree
  :ensure t)
(global-set-key [f8] 'neotree-toggle)

(use-package lsp-ui :commands lsp-ui-mode)

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(consult cursory doom-modeline doom-themes flycheck lsp-ui magit
	     neotree orderless projectile-ripgrep rainbow-delimiters
	     vertico xcscope)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
