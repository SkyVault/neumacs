;;; some-nice-colors-theme.el --- some-nice-colors
;;; Version: 1.0
;;; Commentary:
;;; A theme called some-nice-colors
;;; Code:

(deftheme some-nice-colors "DOCSTRING for some-nice-colors")
  (custom-theme-set-faces 'some-nice-colors
   '(default ((t (:foreground "#c4b3ad" :background "#000000" ))))
   '(cursor ((t (:background "#fdf4c1" ))))
   '(fringe ((t (:background "#282828" ))))
   '(mode-line ((t (:foreground "#bccc9a" :background "#111111" ))))
   '(region ((t (:background "#501825" ))))
   '(secondary-selection ((t (:background "#3e3834" ))))
   '(font-lock-builtin-face ((t (:foreground "#bccc9a" ))))
   '(font-lock-comment-face ((t (:foreground "#727272" ))))
   '(font-lock-function-name-face ((t (:foreground "#b97a95" ))))
   '(font-lock-keyword-face ((t (:foreground "#bccc9a" ))))
   '(font-lock-string-face ((t (:foreground "#ffe1af" ))))
   '(font-lock-type-face ((t (:foreground "#F6AE99" ))))
   '(font-lock-constant-face ((t (:foreground "#b97a95" ))))
   '(font-lock-variable-name-face ((t (:foreground "#a2cdcd" ))))
   '(minibuffer-prompt ((t (:foreground "#ffd187" :bold t ))))
   '(font-lock-warning-face ((t (:foreground "#be4222" :bold t ))))
   '(highlight-numbers-number ((t (:foreground "#b97a95"))))
   )

;;;###autoload
(and load-file-name
    (boundp 'custom-theme-load-path)
    (add-to-list 'custom-theme-load-path
                 (file-name-as-directory
                  (file-name-directory load-file-name))))
;; Automatically add this theme to the load path

(provide-theme 'some-nice-colors)

;;; some-nice-colors-theme.el ends here
