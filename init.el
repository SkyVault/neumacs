(setq config-file (expand-file-name "config.el" user-emacs-directory))

(when (file-exists-p config-file)
  (load config-file))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("bf9655cc42b5aae74b1c889069c0116694238ad79561eac615d5b0f7bca7418a" default))
 '(package-selected-packages
   '(consult marginalia ligature fira-code-mode nim-mode ag treemacs-persp treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil treemacs rainbow-mode gdscript-mode which-key magit evil-collection evil moe-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
