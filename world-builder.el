(require 'widget)
(require 'org)

(eval-when-compile (require 'wid-edit))

(setq world-builder-root-directory "~/.emacs.d/beyond-age-world/")
(setq world-builder-buffer-name "world-builder")
(setq world-builder-buffer nil)

(setq current-world nil)

(setq debug-on-error t)

(defun my-directory-files (directory &optional full match nosort)
  "Like `directory-files', but excluding \".\" and \"..\"."
  (let* ((files (cons nil (directory-files directory full match nosort)))
         (parent files)
         (current (cdr files))
         (exclude (list "." ".."))
         (file nil))
    (while (and current exclude)
      (setq file (car current))
      (if (not (member file exclude))
          (setq parent current)
        (setcdr parent (cdr current))
        (setq exclude (delete file exclude)))
      (setq current (cdr current)))
    (cdr files)))

(defun get-list-of-worlds ()
  (my-directory-files world-builder-root-directory))

(defmacro create-editable-field (format variable)
  `(widget-create
    'editable-field
    :format ,format
    :notify (lambda (widget &rest ignore) (setq ,variable (widget-value widget)))
    ""))

(defun generate-world-builder-buffer ()
  (when world-builder-buffer
    (switch-to-buffer-other-window "world-builder")
    (kill-buffer))
  (setq world-builder-buffer (get-buffer-create world-builder-buffer-name))
  (switch-to-buffer-other-window "world-builder")
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun switch-page-to (page)
  (let ((inhibit-read-only t))
    ;; In the future we could clear the buffer
    (generate-world-builder-buffer)
    (funcall page)))

(defun on-submit (el &rest ig)
  (switch-page-to 'create-new-world))

(defun on-cancel (el &rest ig)
  (switch-page-to 'main-page))

(defun new-world-page ()
  (setq name "")
  (setq description "")

  (defun create-new-world ()
    (let* ((world-dir (concat world-builder-root-directory "/" name))
	   (world-file (concat world-dir "/" name ".org")))

      (if (file-directory-p world-dir)
	(message "World of that name already exists!" world-dir)
	(progn
	  (make-directory world-dir)
	  (write-region
	   (concat "#+TITLE: " name "\n" "#+DESCRIPTION: " description "\n")
	   nil
	   world-file
	   'append)))))

  (widget-insert "New world form.\n")

  (create-editable-field "\nName: %v\n" name)
  (create-editable-field "Description: %v\n" description)

  (widget-insert "\n")

  (widget-create 'push-button :notify 'on-submit "Submit")
  (widget-create 'push-button :notify 'on-cancel "Cancel")

  (widget-setup))

(defun edit-world-page ()
  )

(defun main-page ()
  (widget-insert "Dustin's World Builder and Database.\n\n")
  (widget-create 'push-button :notify (lambda (el &rest ig) (switch-page-to 'new-world-page)) "+ New World")

  (widget-insert "\n\n=== Select a world to Edit ===\n")

  (let ((items (get-list-of-worlds)))
    (dolist (item items)
      (widget-create
       'push-button
       :notify (lambda (el &rest ig) (switch-page-to 'edit-world-page))
       (concat item))
      (insert "\n"))))

(defun reload-wb ()
  (interactive)
  (eval-buffer)
  (start-world-builder))

(global-set-key (kbd "C-c C-c") 'reload-wb)

(defun start-world-builder ()
  (interactive)

  (lisp-mode)
  (remove-overlays)
  (generate-world-builder-buffer)

  (main-page))
