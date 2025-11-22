;;; init-diary.el --- Custom configuration of diary. -*- lexical-binding: t; -*-

(require 'calendar)
(calendar-set-date-style 'iso)
(setq calendar-chinese-all-holidays-flag t
      calendar-minimum-window-height 8)
(add-hook 'calendar-today-visible-hook #'calendar-mark-today)
(add-hook 'calendar-mode-hook (lambda () (toggle-truncate-lines 1)))

(require 'diary-lib)
(setq diary-display-function #'diary-fancy-display
      diary-number-of-entries 7
      diary-file (concat emacs-dir "diary")
      diary-show-holidays-flag nil
      calendar-mark-diary-entries-flag t
      diary-list-include-blanks t)
(add-hook 'diary-list-entries-hook #'diary-sort-entries t)
(add-hook 'diary-list-entries-hook #'diary-include-other-diary-files)
(add-hook 'diary-mark-entries-hook #'diary-mark-included-diary-files)
(add-hook 'diary-nongregorian-listing-hook #'diary-chinese-list-entries)
(add-hook 'diary-nongregorian-listing-hook #'diary-chinese-mark-entries)
(add-hook 'diary-fancy-display-mode-hook (lambda () (text-scale-set -2)))

(require 'appt)
(setq appt-display-mode-line t
      appt-message-warning-time 30
      appt-display-interval 15)
(appt-activate t)

;;; routine
(define-derived-mode routine-mode org-mode "routine")
(defvar routine-tool-bar-map
  (let ((tool-bar-map (make-sparse-keymap)))
    (tool-bar-add-item "close" 'kill-current-buffer 'close)
    (tool-bar-add-item "undo" 'undo 'undo)
    (tool-bar-add-item "save" 'save-buffer 'save)
    (tool-bar-add-item "done" 'routine/yesterday 'yesterday)
    (tool-bar-add-item "done" 'routine/done 'complete)
    (tool-bar-add-item "diary" 'diary 'diary)
    tool-bar-map))
(add-hook 'routine-mode-hook (lambda () (setq-local tool-bar-map routine-tool-bar-map)))

(defun routine/visit-routine-file ()
  (interactive)
  (switch-to-buffer (find-file-noselect (concat emacs-dir "routines.csv")))
  (routine-mode))

(defun routine/done ()
  (interactive)
  (let* ((line-str (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
         (parsed-line (split-string line-str ", " t ","))
         (today-str (format-time-string "%Y-%m-%d")))
    (save-excursion
      (delete-line)
      (insert (car parsed-line) ", " (cadr parsed-line) ", " today-str ?\n))))

(defun routine/yesterday ()
  (interactive)
  (let* ((line-str (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
         (parsed-line (split-string line-str ", " t ","))
         (yesterday-str (format-time-string "%Y-%m-%d" (time-subtract (current-time) (days-to-time 1)))))
    (save-excursion
      (delete-line)
      (insert (car parsed-line) ", " (cadr parsed-line) ", " yesterday-str ?\n))))

(defun routine/check-line ()
  (let* ((line-str (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
         (parsed-line (split-string line-str ", "))
         (today-str (format-time-string "%Y-%m-%d 00:00:00"))
         (today (time-to-days (encode-time (parse-time-string today-str)))))
    (if (or (= 2 (length parsed-line)) (string= "" (car (last parsed-line))))
        (concat (car parsed-line) ": never")
      (let* ((last-time (car (last parsed-line)))
             (last-time-str (concat last-time " 00:00:00"))
             (last-time-day (time-to-days (encode-time (parse-time-string last-time-str))))
             (day-diff (- today last-time-day))
             (freq (string-to-number (cadr parsed-line)))
             (overdue (- day-diff freq)))
        (if (>= overdue 0)  ; things that are due today also count
            (concat (car parsed-line) ": " (number-to-string overdue) "d"))))))

(defun routine/check-and-warn ()
  (let ((header-str ""))
    (with-current-buffer (find-file-noselect (concat emacs-dir "routines.csv"))
      (goto-char (point-min))
      (while (not (eobp))
        (when-let ((overdue-task (routine/check-line)))
          (setq header-str (concat header-str overdue-task "; ")))
        (forward-line 1))
      ;; replace the last semicolon with period
      (setq header-str (concat (substring header-str 0 -2) ".")))
    ;; this function will only trigger upon entering diary buffer
    (setq-local header-line-format header-str)))

(add-hook 'diary-fancy-display-mode-hook #'routine/check-and-warn)

(provide 'init-diary)
;;; init-diary.el ends here
