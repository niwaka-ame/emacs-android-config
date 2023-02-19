(defvar emacs-dir "/sdcard/emacs/")
(when (= emacs-major-version 28)
  (setq emacs-dir "/home/yu/emacs-android/"))

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


(require 'todo-mode)
(setq todo-directory (concat emacs-dir "todos/"))

;; tool bar
;; M-x and C-g
(tool-bar-add-item "home" 'execute-extended-command 'Mx :help "execute command")
(tool-bar-add-item "zoom-in" 'delete-other-windows 'max :help "maximise window")
;; utils
(tool-bar-add-item "sort-column-ascending" 'diary 'diary :help "display diary")
(tool-bar-add-item "spell" 'todo-show 'todo :help "todo show")
;; directions
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
