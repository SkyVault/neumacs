(setq config-file (expand-file-name "config.el" user-emacs-directory))

(when (file-exists-p config-file)
  (load config-file))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("61091a2bcccebf814f9af961226c9843a1e70fdef10d3ed9010826356b95a23d" "9ddad768e351fa64bfda9bed751dd3e3a0740a192dbecb6ef845b46870671733" "e9884e6dca4b86ea04d25bfd9479a07d7d314538d826dd9d669df3cfc9296530" "0e6459ea472233fd6d4843deadf64233b6609ba9c93d8d566bfafa42103f9e2d" "9d052292a11eb2fb333c662b7b309da7478f056137adcaeb77476d5734f4ca22" "c6bbc0e1b8191415847a877a99c6d45828c675b0a1d81f0e6ce48155c80351a4" "40410b2e87047a22cda5cacf66481f8eb26c798d2036e1e4ea503370810e7eae" "2b112b16a4fd9c0b02ce4d5684980e06e9a473639c2801b028e64459e8de4284" "6603719a1fdcdd7781434ce20f573d18aba8f40f1ce40f18e7b9a357930f57f0" "f703efe04a108fcd4ad104e045b391c706035bce0314a30d72fbf0840b355c2c" "c3bd1fd366dd0022f8205fa4ef17e40333ee99c60f97def692c13fe8e30b0634" "bf9655cc42b5aae74b1c889069c0116694238ad79561eac615d5b0f7bca7418a" default))
 '(package-selected-packages
   '(ob-shell ob-async rust-mode lsp-treemacs lsp-ui company lsp-mode consult marginalia ligature fira-code-mode nim-mode ag treemacs-persp treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil treemacs rainbow-mode gdscript-mode which-key magit evil-collection evil moe-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
