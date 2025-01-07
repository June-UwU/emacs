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

(normal-erase-is-backspace-mode 0)
(setq make-backup-files nil)

(setq visible-bell t)

(set-face-attribute 'default nil :font "Fira Code Retina" :height 100)
(column-number-mode)
(global-display-line-numbers-mode t)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key "\C-s" 'swiper)

(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; initialize use-package for non-linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(use-package swiper)
(use-package ivy
  :diminish
  :config
  (ivy-mode 1))

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

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1))

(use-package company
  :ensure t
  :config
  (add-to-list 'company-backends 'company-capf)
  (global-company-mode))

(use-package magit
  :ensure t)

(use-package dumb-jump
  :ensure t)

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(use-package lsp-mode
  :ensure t
  :hook ((c-mode . lsp)
	 (c++-mode . lsp)
	 (pythom-mode . lsp))
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  ;; Optional: use Projectile with completion system like Ivy
  ;; (setq projectile-completion-system 'ivy)
  ;; Optional: key bindings
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package all-the-icons)
(use-package neotree
  :ensure t)
(global-set-key [f8] 'neotree-toggle)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
