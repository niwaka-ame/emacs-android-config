;;; fleet.el --- Defining fleet mode. -*- lexical-binding: t; -*-

(require 'org)

(define-derived-mode fleet-mode org-mode "fleet")

(defvar fleet-tool-bar-map
  (let ((tool-bar-map (make-sparse-keymap)))
    (tool-bar-add-item "close" 'kill-current-buffer 'close)
    (tool-bar-add-item "undo" 'undo 'undo)
    (tool-bar-add-item "save" 'save-buffer 'save)
    (tool-bar-add-item "plus" 'fleet/todo-org 'todo)
    (tool-bar-add-item "done" 'fleet/done-org 'done)
    tool-bar-map))
(add-hook 'fleet-mode-hook (lambda () (setq-local tool-bar-map fleet-tool-bar-map)))

(defun fleet/todo-org (&optional no-switch)
  (interactive)
  (with-current-buffer (find-file-noselect (concat emacs-dir "todo.org"))
    (fleet-mode)
    (goto-char (point-min))
    (org-insert-heading)
    (save-excursion
      (newline)
      (insert (format-time-string "<%F %a>" (current-time))))
    (unless no-switch
      (switch-to-buffer (current-buffer)))))

(defun fleet/done-org ()
  (interactive)
  (org-refile nil nil '("" "done.org" nil nil))
  (save-buffer)
  (save-excursion
    (with-current-buffer (find-file-noselect (concat emacs-dir "done.org"))
      (save-buffer))))

(defun fleet/todo-visit ()
  (interactive)
  (switch-to-buffer (find-file-noselect (concat emacs-dir "todo.org")))
  (fleet-mode))

(defun fleet/add-region ()
  (interactive)
  (if (region-active-p)
      (let* ((beg (region-beginning))
             (end (region-end))
             (buf (current-buffer))
             (url (and (eq major-mode 'eww-mode) (plist-get eww-data :url))))
        (copy-region-as-kill beg (1+ end))
        (fleet/todo-org 'no-switch)
        (with-current-buffer "todo.org"
          (if url (progn
                    (insert url)
                    (forward-line)
                    (newline)
                    (if (string-match-p "[\u4e00-\u9fff]" (car kill-ring))
                        (insert (replace-regexp-in-string "\n" "" (car kill-ring)))
                      (insert (replace-regexp-in-string "\n" " " (car kill-ring)))))
            (yank))
          (newline)
          (goto-char (point-min))
          (save-buffer)))
    (message "mark region first!")))

(defun fleet/add-url ()
  (interactive)
  (let ((url (plist-get eww-data :url)))
    (when url
      (fleet/todo-org 'no-switch)
      (with-current-buffer "todo.org"
        (insert url)
        (save-buffer)))))

(provide 'fleet)
;;; fleet.el ends here
