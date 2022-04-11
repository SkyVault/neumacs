;; package --- Dustin's Emacs Config
;;; Commentary:

;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(let ((file (expand-file-name "echo-bell.el" user-emacs-directory)))
  (when (file-exists-p file)
    (load file)
    (echo-bell-mode)))

;; use-package to simplify the config file

(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure 't)

;; User interface

(setq inhibit-startup-message t
      initial-scratch-message "Hello, Dustin.")

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(load-theme 'some-nice-colors t)

(use-package rainbow-mode
  :ensure t
  :config
  (rainbow-mode))

;;; Fonts
;; dnf install fira-code-fonts

;; Set default font

(set-face-attribute
    'default nil
    :family "Fira Code Retina"
    :height 110
    :weight 'normal
    :width 'normal)

(let ((file (expand-file-name "ligature.el" user-emacs-directory)))
  (when (file-exists-p file)
    (load file)
    (ligature-set-ligatures 't '("www"))
    (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
    (ligature-set-ligatures 'prog-mode
			    '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
			    ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
			    "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
			    "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
			    "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
			    "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
			    "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
			    "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
			    ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
			    "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
			    "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
			    "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
			    "\\\\" "://"))
    (global-ligature-mode t)))

(use-package highlight-numbers
  :ensure t
  :config
  (highlight-numbers-mode))

;; Editor behaviour

(setq make-backup-files nil)
(setq visible-bell 1)

(setq browse-url-browser-function 'eww-browse-url)

(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode))

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(defun open-config-file ()
  (interactive)
  (find-file (expand-file-name "config.el" user-emacs-directory)))

(global-set-key (kbd "C-c C-c C-e") 'open-config-file)

;;; Projectile

(use-package ag
  :ensure t
  :config
  (global-set-key (kbd "C-x /") 'ag))

(use-package projectile
  :after ag
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-find-file))

;;; Treemacs

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   t
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

;;; Vertico

(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; This controls how vertico orders the lines
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package consult
  :ensure t
  :config
  (global-set-key (kbd "C-x b") 'consult-buffer))

(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

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
(setq org-todo-keywords
'((sequence "IDEA(i)" "TODO(t)" "STARTED(s)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)")
  (sequence "|" "CANCELED(c)" "DELEGATED(l)" "SOMEDAY(f)")))

(setq org-todo-keyword-faces
  '(("IDEA" . (:foreground "GoldenRod" :weight bold))
    ("NEXT" . (:foreground "IndianRed1" :weight bold))
    ("STARTED" . (:foreground "OrangeRed" :weight bold))
    ("WAITING" . (:foreground "coral" :weight bold))
    ("CANCELED" . (:foreground "LimeGreen" :weight bold))
    ("DELEGATED" . (:foreground "LimeGreen" :weight bold))
    ("SOMEDAY" . (:foreground "LimeGreen" :weight bold))))

(setq org-tag-persistent-alist
  '((:startgroup . nil)
    ("HOME" . ?h)
    ("RESEARCH" . ?r)
    ("TEACHING" . ?t)
    (:endgroup . nil)
    (:startgroup . nil)
    ("OS" . ?o)
    ("DEV" . ?d)
    ("WWW" . ?w)
    (:endgroup . nil)
    (:startgroup . nil)
    ("EASY" . ?e)
    ("MEDIUM" . ?m)
    ("HARD" . ?a)
    (:endgroup . nil)
    ("UCANCODE" . ?c)
    ("URGENT" . ?u)
    ("KEY" . ?k)
    ("BONUS" . ?b)
    ("noexport" . ?x)))

(setq org-tag-faces
  '(("HOME" . (:foreground "GoldenRod" :weight bold))
    ("RESEARCH" . (:foreground "GoldenRod" :weight bold))
    ("TEACHING" . (:foreground "GoldenRod" :weight bold))
    ("OS" . (:foreground "IndianRed1" :weight bold))
    ("DEV" . (:foreground "IndianRed1" :weight bold))
    ("WWW" . (:foreground "IndianRed1" :weight bold))
    ("URGENT" . (:foreground "Red" :weight bold))
    ("KEY" . (:foreground "Red" :weight bold))
    ("EASY" . (:foreground "OrangeRed" :weight bold))
    ("MEDIUM" . (:foreground "OrangeRed" :weight bold))
    ("HARD" . (:foreground "OrangeRed" :weight bold))
    ("BONUS" . (:foreground "GoldenRod" :weight bold))
    ("UCANCODE" . (:foreground "GoldenRod" :weight bold))
    ("noexport" . (:foreground "LimeGreen" :weight bold))))

(setq org-html-coding-system 'utf-8-unix)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-html-validation-link nil)

(setq org-log-done t)

(use-package org-bullets
  :ensure t)

(defun my/buffer-face-mode-variable ()
  "Set font to a variable width (proportional) fonts in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "Roboto Slab"
                                :height 150
                                :width normal))
  (buffer-face-mode))

(defun my/set-general-faces-org ()
  (org-indent-mode 1)
  (my/buffer-face-mode-variable)
  (setq line-spacing 0.1
        org-pretty-entities t
        org-startup-indented t
        org-adapt-indentation nil)
  (variable-pitch-mode +1)
  (mapc
   (lambda (face) ;; Other fonts that require it are set to fixed-pitch.
     (set-face-attribute face nil :inherit 'fixed-pitch))
   (list 'org-block
         'org-table
         'org-verbatim
         'org-block-begin-line
         'org-block-end-line
         'org-meta-line
         'org-date
         'org-drawer
         'org-property-value
         'org-special-keyword
         'org-document-info-keyword))
  (mapc ;; This sets the fonts to a smaller size
   (lambda (face)
     (set-face-attribute face nil :height 0.8))
   (list 'org-document-info-keyword
         'org-block-begin-line
         'org-block-end-line
         'org-meta-line
         'org-drawer
         'org-property-value
         )))

(defun my/set-specific-faces-org ()
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-level-1 nil :height 1.35 :foreground "#bccc9a" :box nil :underline nil)
  (set-face-attribute 'org-level-2 nil :height 1.25 :foreground "#b97a95" :slant 'italic)
  (set-face-attribute 'org-level-3 nil :height 1.1 :foreground "#F6AE99" :slant 'italic)
  (set-face-attribute 'org-level-4 nil :height 1.05 :foreground "#A2CDCD")
  (set-face-attribute 'org-level-5 nil :foreground "#b97a95")
  (set-face-attribute 'org-date nil :foreground "#ECBE7B" :height 0.8)
  (set-face-attribute 'org-document-title nil :foreground "#b97a95" :height 1.75 :underline t)
  (set-face-attribute 'org-ellipsis nil :foreground "#4f747a" :underline nil)
  (set-face-attribute 'variable-pitch nil :family "Roboto Slab" :height 1.2))

(defun my/style-org ()
  (interactive)
  (my/set-general-faces-org)
  (my/set-specific-faces-org)
  ;; (my/set-keyword-faces-org)
  )

(add-hook 'org-mode-hook 'my/style-org)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(defun open-work-notes ()
  (interactive)
  (message "new note")
;;  "%Y-%m-%dT%H:%M:%S"
  (let ((today (format-time-string "%b %d %Y"))
	(file-name (-concat )))
    (message today)))

(global-set-key (kbd "C-c o w") 'open-work-notes)

;; Company mode

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t))

;; Rust

(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t))

;; Godot

(use-package gdscript-mode
  :ensure t)

;; Nim

(use-package nim-mode
  :ensure t
  :config
  (defun init-nim-mode ()
    (auto-fill-mode 0)
    (electric-indent-local-mode 0)

    (setq nimsuggest-path "/home/dustin/Repos/nim-1.6.0/bin/nimsuggest")

    (nimsuggest-mode)
    ;;(flycheck-mode 1)

    (setq-default tab-width 2)
    (setq-default indent-tabs-mode nil)
    (defvaralias 'c-basic-offset 'tab-width)
    (defvaralias 'cperl-indent-level 'tab-width)
    )

  (add-hook 'nim-mode-hook 'init-nim-mode))

(use-package highlight-indent-guides
  :ensure t
  :config
  (setq highlight-indent-guides-method 'character)
  (add-hook 'nim-mode-hook 'highlight-indent-guides-mode))

;; LSP mode

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  (add-hook 'gdscript-mode #'lsp))

(use-package lsp-ui
  :ensure t)

(use-package lsp-treemacs
  :ensure t
  :config
  (lsp-treemacs-sync-mode))

;; glsl
(use-package glsl-mode
  :ensure t
  :config)

;; Fly check

;; (use-package flycheck
;;   :ensure t
;;   :init
;;   (global-flycheck-mode))

(provide 'config)

;; Elfeed (RSS)

(use-package elfeed
  :ensure t
  :config
  (global-set-key (kbd "C-c e") 'elfeed)
  (setq elfeed-feeds
	'("https://lukesmith.xyz/rss.xml"
	  "https://notrelated.libsyn.com/rss"
	  "https://hnrss.org/frontpage"
	  "https://christine.website/blog.rss"
	  "https://www.reuters.com/news/archive/domesticNews"
	  "https://fabiensanglard.net/rss.xml"))
  (elfeed-update))

;;; config.el ends here

