;;; some-nice-colors-theme.el --- some-nice-colors
;;; Version: 1.0
;;; Commentary:
;;; A theme called some-nice-colors
;;; Code:

(deftheme some-nice-colors "DOCSTRING for some-nice-colors")
  (custom-theme-set-faces 'some-nice-colors
   '(default ((t (:foreground "#d2fff5" :background "#000000" ))))
   '(cursor ((t (:background "#fdf4c1" ))))
   '(fringe ((t (:background "#282828" ))))
   '(mode-line ((t (:foreground "#282828" :background "#7c6f64" ))))
   '(region ((t (:background "#504945" ))))
   '(secondary-selection ((t (:background "#3e3834" ))))
   '(font-lock-builtin-face ((t (:foreground "#d68b9f" ))))
   '(font-lock-comment-face ((t (:foreground "#727272" ))))
   '(font-lock-function-name-face ((t (:foreground "#fff6c2" ))))
   '(font-lock-keyword-face ((t (:foreground "#ffd289" ))))
   '(font-lock-string-face ((t (:foreground "#dfc5c5" ))))
   '(font-lock-type-face ((t (:foreground "#d3869b" ))))
   '(font-lock-constant-face ((t (:foreground "#d3869b" ))))
   '(font-lock-variable-name-face ((t (:foreground "#83a598" ))))
   '(minibuffer-prompt ((t (:foreground "#ffd187" :bold t ))))
   '(font-lock-warning-face ((t (:foreground "#fe9220" :bold t ))))
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
