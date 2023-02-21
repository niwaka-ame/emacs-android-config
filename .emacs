(defvar emacs-dir "/sdcard/emacs/")
(when (= emacs-major-version 28)
  (setq emacs-dir "/home/yu/emacs-android/"))

;; load theme
(load-theme 'tango-dark t)

;;; larger font on phone
(set-face-attribute 'default nil :height 200)

;;; set up a splash screen for diary

(setq inhibit-startup-screen t)

(require 'calendar)
(calendar-set-date-style 'iso)
(setq calendar-chinese-all-holidays-flag t
      calendar-minimum-window-height 12)
(calendar)

(require 'diary-lib)
(setq diary-display-function #'diary-fancy-display
      diary-number-of-entries 7
      diary-file (concat emacs-dir "diary")
      diary-show-holidays-flag nil)
(add-hook 'diary-list-entries-hook #'diary-sort-entries t)
(add-hook 'diary-list-entries-hook #'diary-include-other-diary-files)
(add-hook 'diary-mark-entries-hook #'diary-mark-included-diary-files)
(add-hook 'diary-nongregorian-listing-hook #'diary-chinese-list-entries)
(add-hook 'diary-nongregorian-listing-hook #'diary-chinese-mark-entries)
(calendar-goto-today)
(diary)
(calendar-exit)

(require 'appt)
(setq appt-display-mode-line t
      appt-message-warning-time 30)
(appt-activate t)


;; use org files for fleeting notes
(defun fleet-todo-org ()
  (interactive)
  (with-current-buffer (find-file-noselect (concat emacs-dir "todo.org"))
    (goto-char (point-min))
    (org-insert-heading)
    (save-excursion
      (newline)
      (insert (format-time-string "<%F %a>" (current-time))))
    (display-buffer (current-buffer))
    (switch-to-buffer (current-buffer))))

(defun fleet-done-org ()
  (interactive)
  (org-refile nil nil '("" "done.org" nil nil))
  (save-buffer)
  (save-excursion
    (with-current-buffer (find-file-noselect (concat emacs-dir "done.org"))
      (save-buffer))))

(defun fleet-todo-visit ()
  (interactive)
  (switch-to-buffer (find-file-noselect (concat emacs-dir "todo.org"))))

;; tool bar
;; M-x and C-g
(tool-bar-add-item "home" 'execute-extended-command 'Mx :help "execute command")
(tool-bar-add-item "zoom-in" 'delete-other-windows 'max :help "maximise window")
;; utils
(tool-bar-add-item "sort-column-ascending" 'diary 'diary :help "display diary")
(tool-bar-add-item "spell" 'fleet-todo-visit 'visit :help "visit todo")
(tool-bar-add-item "preferences" 'eshell 'eshell :help "eshell")
(tool-bar-add-item "separator" nil 'Nil)
(tool-bar-add-item "sort-criteria" 'fleet-todo-org 'todo :help "new todo")
(tool-bar-add-item "info" 'fleet-done-org 'done :help "done todo")
;; directions
(tool-bar-add-item "separator" nil nil)
(tool-bar-add-item "left-arrow" 'backward-char 'bw :help "backward char")
(tool-bar-add-item "up-arrow" 'previous-line 'up :help "previous line")
(tool-bar-add-item "sort-ascending" 'next-line 'down :help "next line")
(tool-bar-add-item "right-arrow" 'forward-char 'fw :help "forward char")

;; mode line
(setq-default mode-line-format
      '("%e" mode-line-front-space mode-line-modes " " mode-line-buffer-identification " " mode-line-misc-info " " mode-line-position
  mode-line-end-spaces))

;; menu
(define-key global-map
  [menu-bar edit set-mark]
  '("Set mark" . set-mark-command))

; (add-hook 'after-init-hook (lambda () (load-theme "wombat" t)))
