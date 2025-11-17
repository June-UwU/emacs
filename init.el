;; -------------------------------
;; Basic Emacs Configuration
;; -------------------------------

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Set default font and theme (optional)
(set-face-attribute 'default nil :height 120)
(load-theme 'wombat t)

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

(use-package cursory
  :ensure t
  :demand t
  :if (display-graphic-p)
  :config
  (setq cursory-presets
        '((box
           :cursor-color success ; will typically be green
           :blink-cursor-interval 1.2)
          (box-no-blink
           :inherit box
           :blink-cursor-mode -1)
          (bar
           :cursor-type (bar . 2)
           :cursor-color error ; will typically be red
           :blink-cursor-interval 0.8)
          (bar-no-other-window
           :inherit bar
           :cursor-in-non-selected-windows nil)
          (bar-no-blink
           :inherit bar
           :blink-cursor-mode -1)
          (underscore
           :cursor-color warning ; will typically be yellow
           :cursor-type (hbar . 3)
           :blink-cursor-interval 0.3
           :blink-cursor-blinks 50)
          (underscore-no-other-window
           :inherit underscore
           :cursor-in-non-selected-windows nil)
          (underscore-thick
           :inherit underscore
           :cursor-type (hbar . 8)
           :cursor-in-non-selected-windows (hbar . 3))
          (t ; the default values
           :cursor-color unspecified ; use the theme's original
           :cursor-type box
           :cursor-in-non-selected-windows hollow
           :blink-cursor-mode 1
           :blink-cursor-blinks 10
           :blink-cursor-interval 0.2
           :blink-cursor-delay 0.2)))

  ;; I am using the default value of `cursory-latest-state-file'.

  ;; Set last preset or fall back to desired style from
  ;; `cursory-presets'.  Alternatively, use the function
  ;; `cursory-set-last-or-fallback' (can be added to the
  ;; `after-init-hook'.
  (cursory-set-preset (or (cursory-restore-latest-preset) 'box))

  ;; Persist configurations between Emacs sessions.  Also apply the
  ;; :cursor-color again when swithcing to another theme.
  (cursory-mode 1))
;; -------------------------------
;; Optional: UI Tweaks
;; -------------------------------

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(consult cursory magit projectile-ripgrep vertico xcscope)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun my/generate-and-propagate-cscope-db ()
  "Generate cscope database in the project root and copy cscope.out into every subdirectory.
Project root is taken from `projectile-project-root' if available, otherwise `default-directory'.
Skips .git, .svn, .hg, node_modules, build, dist directories."
  (interactive)
  (let* ((root (or (and (fboundp 'projectile-project-root) (projectile-project-root))
                   default-directory))
         (db-file "cscope.out")
         (files-cmd "find . \\( -name \"*.c\" -o -name \"*.cc\" -o -name \"*.cpp\" -o -name \"*.cxx\" -o -name \"*.hpp\" \\) > cscope.files")
         ;; copy command: find all dirs, exclude common dirs, then copy cscope.out into each dir
         (copy-cmd "find . -type d \\( -path './.git' -o -path './.git/*' -o -path './.svn' -o -path './.svn/*' -o -path './.hg' -o -path './.hg/*' -o -path './node_modules' -o -path './node_modules/*' -o -path './build' -o -path './build/*' -o -path './dist' -o -path './dist/*' \\) -prune -o -type d -print0 | xargs -0 -I{} cp -f ./cscope.out \"{}\"")
    (unless (and root (file-directory-p root))
      (user-error "Project root not found"))
    (let ((default-directory (file-name-as-directory root)))
      ;; generate file list
      (with-current-buffer (get-buffer-create "*cscope-build*")
        (erase-buffer))
      (let ((rc1 (call-process-shell-command files-cmd nil "*cscope-build*" t)))
        (unless (zerop rc1)
          (message "Failed to generate cscope.files (exit %d). See *cscope-build*." rc1)
          (pop-to-buffer "*cscope-build*")
          (cl-return-from my/generate-and-propagate-cscope-db nil)))
      ;; run cscope build
      (with-current-buffer (get-buffer-create "*cscope-build*")
        (goto-char (point-max)))
      (let ((rc2 (call-process-shell-command "cscope -b -q -k" nil "*cscope-build*" t)))
        (unless (and (zerop rc2) (file-exists-p (expand-file-name db-file default-directory)))
          (message "cscope build failed (exit %d) or %s missing. See *cscope-build*." rc2 db-file)
          (pop-to-buffer "*cscope-build*")
          (cl-return-from my/generate-and-propagate-cscope-db nil)))
      ;; propagate
      (with-current-buffer (get-buffer-create "*cscope-propagate*")
        (erase-buffer))
      (let ((rc3 (call-process-shell-command copy-cmd nil "*cscope-propagate*" t)))
        (if (zerop rc3)
            (progn
              (message "cscope.out generated and propagated to subdirectories under %s" root)
              t)
          (message "Propagation failed (exit %d). See *cscope-propagate*." rc3)
          (pop-to-buffer "*cscope-propagate*")
          nil)))))

(global-set-key (kbd "M-g") #'my/generate-and-propagate-cscope-db)
