(defvar emacs-dir "/sdcard/emacs/kawa/")
(when (= emacs-major-version 28)
  (setq emacs-dir "/home/yu/emacs-android/"))

;; load theme
(load-theme 'tango-dark t)

;;; larger font on phone
(set-face-attribute 'default nil :height 220)
(require 'face-remap)
(setq text-scale-mode-step 1.1)

;;; set up a splash screen for diary

(setq inhibit-startup-screen t
      initial-scratch-message nil)

(unless (= emacs-major-version 28)
  (add-to-list 'load-path "/sdcard/emacs/emacs-android-config/")
  (require 'stardict)
  (setq stardict-dir "/sdcard/emacs/langdao/"
        stardict-name "langdao-ec-gb")
  (load "init-packages.el"))

;; some useful builtin mode
(require 'delsel)
(delete-selection-mode)

(require 'paren)
(setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)
(show-paren-mode)

;;; calendar, diary and appointment
(require 'calendar)
(calendar-set-date-style 'iso)
(setq calendar-chinese-all-holidays-flag t
      calendar-minimum-window-height 8)
(add-hook 'calendar-today-visible-hook #'calendar-mark-today)
(add-hook 'calendar-mode-hook (lambda () (toggle-truncate-lines 1)))
(add-hook 'calendar-initial-window-hook #'delete-other-windows)

(require 'diary-lib)
(setq diary-display-function #'diary-fancy-display
      diary-number-of-entries 7
      diary-file (concat emacs-dir "diary")
      diary-show-holidays-flag nil
      calendar-mark-diary-entries-flag t)
(add-hook 'diary-list-entries-hook #'diary-sort-entries t)
(add-hook 'diary-list-entries-hook #'diary-include-other-diary-files)
(add-hook 'diary-mark-entries-hook #'diary-mark-included-diary-files)
(add-hook 'diary-nongregorian-listing-hook #'diary-chinese-list-entries)
(add-hook 'diary-nongregorian-listing-hook #'diary-chinese-mark-entries)
(add-hook 'diary-mode-hook #'variable-pitch-mode)
(add-hook 'diary-fancy-display-mode-hook #'variable-pitch-mode)
(add-hook 'diary-fancy-display-mode-hook
          (lambda () (switch-to-buffer "*Fancy Diary Entries*") (delete-other-windows)))

(require 'appt)
(setq appt-display-mode-line t
      appt-message-warning-time 30)
(appt-activate t)

;; internet
(require 'browse-url)
(require 'webjump)
(require 'eww)
(setq browse-url-browser-function 'eww)
(setq webjump-sites
      '(("ddg" . [simple-query "duckduckgo.com" "duckduckgo.com/?q=" ""])
        ("gsc" . [simple-query "scholar.google.com" "scholar.google.com/scholar?q=" ""])))
(setq eww-bookmarks-directory emacs-dir
      shr-inhibit-images t)

;; RSS
(require 'newsticker)
(setq
     newsticker-url-list-defaults nil
     newsticker-retrieval-interval 0
     newsticker-automatically-mark-items-as-old nil
     newsticker-obsolete-item-max-age (* 2 86400)
     newsticker-frontend 'newsticker-plainview)
; RSS feed list in a separate file
(load (concat emacs-dir "rssfeeds.el"))
(add-hook 'newsticker-mode-hook #'variable-pitch-mode)

;; truncate line in orgmode
(require 'org)
(add-hook 'org-mode-hook (lambda () (toggle-truncate-lines -1)))
(add-hook 'org-mode-hook #'variable-pitch-mode)
(add-hook 'org-mode-hook #'org-indent-mode)
(setq org-cite-global-bibliography (list "/home/yu/denote/lib.bib"))

;; use org files for fleeting notes
(defun fleet-todo-org (&optional no-switch)
  (interactive)
  (with-current-buffer (find-file-noselect (concat emacs-dir "todo.org"))
    (goto-char (point-min))
    (org-insert-heading)
    (save-excursion
      (newline)
      (insert (format-time-string "<%F %a>" (current-time))))
    (unless no-switch
      (switch-to-buffer (current-buffer)))))

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

(defun fleet-add-region ()
  (interactive)
  (let* ((regionp (region-active-p))
         (beg (and regionp (region-beginning)))
         (end (and regionp (region-end)))
         (buf (current-buffer))
         (url (and (string= (buffer-name) "*eww*") (plist-get eww-data :url))))
    (when regionp
      (fleet-todo-org 'no-switch)
      (with-current-buffer "todo.org"
        (when url
          (insert url)
          (next-line)
          (newline))
        (insert-buffer-substring-no-properties buf beg end))
      (goto-char (point-min))
      (save-buffer))))

(defun fleet-add-url ()
  (interactive)
  (let ((url (plist-get eww-data :url)))
    (when url
      (fleet-todo-org 'no-switch)
      (with-current-buffer "todo.org"
        (insert url)
        (save-buffer)))))

;; glossary
(defun glossary-add-at-point ()
  (interactive)
  (let ((word (thing-at-point 'word)))
    (with-current-buffer (find-file-noselect (concat emacs-dir "glossary"))
      (goto-char (point-min))
      (insert (concat word "\n"))
      (save-buffer)
      (kill-buffer))
    (when (string= (buffer-name) "*stardict*")
      (kill-buffer))))

(defun glossary-visit ()
  (interactive)
  (switch-to-buffer (find-file-noselect (concat emacs-dir "glossary"))))

;; tool bar
(tool-bar-add-item "home" 'execute-extended-command 'Mx :help "execute command")
(tool-bar-add-item "zoom-in" 'delete-other-windows 'max :help "maximise window")
;; utils
(tool-bar-add-item "sort-column-ascending" 'diary 'diary :help "display diary")
(tool-bar-add-item "sort-descending" 'fleet-todo-visit 'visit :help "visit todo")
(tool-bar-add-item "spell" 'glossary-visit 'glossary :help "visit glossary")
(tool-bar-add-item "describe" 'newsticker-show-news 'news :help "News ticker")
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
; remove two menus
(define-key global-map [menu-bar options] nil)
(define-key global-map [menu-bar tools] nil)
; add my own menu
(define-key-after
  (lookup-key global-map [menu-bar])
  [my]
  (cons "My" (make-sparse-keymap "My"))
  'buffers)
; zoom in and out
(define-key global-map
  [menu-bar my text-scale-increase]
  '("zoom in" . text-scale-increase))
(define-key global-map
  [menu-bar my text-scale-decrease]
  '("zoom out" . text-scale-decrease))
; tools
(define-key global-map
  [menu-bar my visit-init]
  '("visit .emacs file" .
    (lambda ()
      (interactive)
      (find-file-noselect "~/.emacs")
      (switch-to-buffer ".emacs"))))
(define-key global-map
  [menu-bar my ielm]
  '("ielm" . ielm))
(define-key global-map
  [menu-bar my eshell]
  '("eshell" . eshell))
(define-key global-map
  [menu-bar my calendar]
  '("calendar" . calendar))
; EWW series
(define-key global-map
  [menu-bar my fleet-add-region]
  '("copy region to fleet note" . fleet-add-region))
(define-key global-map
  [menu-bar my fleet-add-url]
  '("copy URL to fleet note" . fleet-add-url))
(define-key global-map
  [menu-bar my webjump]
  '("web jump" . webjump))
(define-key global-map
  [menu-bar my eww-readable]
  '("EWW readable" . eww-readable))
(define-key global-map
  [menu-bar my eww-list-bookmarks]
  '("EWW bookmark" . eww-list-bookmarks))
(define-key global-map
  [menu-bar my eww]
  '("EWW" . eww))
; dictionary
(define-key global-map
  [menu-bar my glossary-add-at-point]
  '("Add to glossary" . glossary-add-at-point))
(define-key global-map
  [menu-bar my stardict-define]
  '("Define word" . stardict-define))
(define-key global-map
  [menu-bar my stardict-define-at-point]
  '("Define at point" . stardict-define-at-point))


;; third-party packages
(unless (= emacs-major-version 28)
  ;; pangu-spacing
  (require 'pangu-spacing)
  (add-hook 'org-mode-hook #'pangu-spacing-mode)
  ;; denote
  (require 'denote)
  (setq denote-directory "/sdcard/emacs/denote/"
        denote-backlinks-show-context t)
  (setq denote-known-keywords
        '(life philosophy gedanken biology cs maths physics economics politics history))
  (defun visit-random-denote ()
    (interactive)
    (let* ((filelist (remove "lib.bib" (cddr (directory-files denote-directory))))
           (numfiles (length filelist)))
      (switch-to-buffer
       (find-file-noselect
        (concat denote-directory
                (elt filelist (random numfiles)))))))
  (define-key global-map
    [menu-bar my random-denote]
    '("visit random note" . visit-random-denote))
  )


;; finalise startup apperance
(with-current-buffer "*scratch*"
  (org-mode))
(switch-to-buffer "*Fancy Diary Entries*") ; generated by appt
(delete-other-windows)
