;; Package setup

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; use-package to simplify the config file

(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure 't)

;; User interface

(setq inhibit-startup-message t
      initial-scratch-message "Welcome Dustin!")

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Editor behaviour

(setq make-backup-files nil)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;; Theme

(use-package moe-theme
  :config
  (require 'moe-theme)
  (moe-dark))

;; Evil mode

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

;; Magit

(use-package magit
  :ensure t
  :config)

;; Org mode
