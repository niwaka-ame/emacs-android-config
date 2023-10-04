(defvar emacs-dir "/sdcard/emacs/kawa/")

;; load theme
(load-theme 'tango-dark t)

;;; larger font on phone
(set-face-attribute 'default nil :height 220)
(require 'face-remap)
(setq text-scale-mode-step 1.1)

;;; set up a splash screen for diary

(setq inhibit-startup-screen t
      initial-scratch-message nil)

(add-to-list 'load-path "/sdcard/emacs/emacs-android-config/")
(let ((default-directory "/sdcard/emacs/emacs-android-config/"))
  (normal-top-level-add-subdirs-to-load-path))

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

;; org-habit
(add-to-list 'org-modules 'org-habit)
(setq org-agenda-files (list (concat emacs-dir "habits.org"))
      org-habit-show-all-today t
      org-agenda-span 1
      org-agenda-start-day "+0d"
      org-habit-preceding-days 14
      org-habit-following-days 6
      org-habit-graph-column 27
      )
(add-hook 'org-agenda-mode-hook #'delete-other-windows)

(defun habit/org-habit-done ()
  (interactive)
  (org-todo 'done))

(defun habit/visit-habit-file ()
  (interactive)
  (switch-to-buffer (find-file-noselect (concat emacs-dir "habits.org"))))

(defun habit/add-habit (habit freq)
  (interactive "sHabit: \nsFrequency: ")
  (with-current-buffer (find-file-noselect (concat emacs-dir "habits.org"))
    (org-mode)
    (goto-char (point-max))
    (newline)
    (org-insert-heading)
    (insert habit)
    (save-excursion
      (newline)
      (insert (concat
               "SCHEDULED:"
               (format-time-string "<%F %a " (current-time))
               freq
               ">")))
    (org-set-property "STYLE" "habit")
    (switch-to-buffer (current-buffer))))

;; use org files for fleeting notes
(defun fleet/todo-org (&optional no-switch)
  (interactive)
  (with-current-buffer (find-file-noselect (concat emacs-dir "todo.org"))
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
  (switch-to-buffer (find-file-noselect (concat emacs-dir "todo.org"))))

(defvar fleet/region nil)

(defun fleet/mark-region ()
  (interactive)
  (push (point) fleet/region))

(defun fleet/add-region ()
  (interactive)
  (if (>= (length fleet/region) 2)
      (let* ((beg (nth (- (length fleet/region) 2) fleet/region))
             (end (nth (- (length fleet/region) 1) fleet/region))
             (buf (current-buffer))
             (url (and (string= (buffer-name) "*eww*") (plist-get eww-data :url))))
        (copy-region-as-kill beg end)
        (setq fleet/region nil)
        (fleet/todo-org 'no-switch)
        (with-current-buffer "todo.org"
          (when url
            (insert url)
            (forward-line)
            (newline))
          (yank)
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

;; glossary
(require 'stardict)
(defun glossary/add-at-point ()
  (interactive)
  (let ((word (thing-at-point 'word)))
    (with-current-buffer (find-file-noselect (concat emacs-dir "glossary"))
      (goto-char (point-min))
      (insert (concat word "\n"))
      (save-buffer)
      (kill-buffer))
    (when (string= (buffer-name) "*stardict*")
      (kill-buffer))))

(defun glossary/visit ()
  (interactive)
  (switch-to-buffer (find-file-noselect (concat emacs-dir "glossary"))))

(defun glossary/revisit ()
  (interactive)
  (let ((word-number 5))
    (with-current-buffer (find-file-noselect (concat emacs-dir "glossary"))
      (let ((total-word-number
             (count-lines (point-min) (point-max))))
        (dotimes (i word-number)
          (goto-char (point-min))
          (let ((line (random total-word-number)))
            (forward-line line)
            (let ((word (downcase (thing-at-point 'word))))
              (with-current-buffer (get-buffer-create "*glossary-revisit*")
                (when (= i 0) (erase-buffer))
                (goto-char (point-max))
                (when (> i 0) (newline))
                (insert (stardict--lookup-and-return word))
                (newline))))))))
  (switch-to-buffer "*glossary-revisit*")
  (goto-char (point-min)))

(defun glossary/flow (word)
  "Prompt for a word continuously."
  (interactive "sWord: ")
  (stardict--load-dict)
  (while (not (string= word "q")) ; "q" means quit
    (setq word (string-trim (downcase word)))
    (if (stardict-word-exist-p stardict-dict-hash word)
        (stardict--lookup-and-display word)
      (message "No definition is found!"))
    (let ((str (read-string "Word: ")))
      (if (string= str "a") ; "a" means add to glossary
          (with-current-buffer (find-file-noselect (concat emacs-dir "glossary"))
            (goto-char (point-min))
            (insert (concat word "\n"))
            (save-buffer)
            (kill-buffer))
        ;; othewise update `WORD'
        (setq word str)))))

;; tool bar
;; (tool-bar-add-item "home" 'execute-extended-command 'Mx :help "execute command")
(tool-bar-add-item "zoom-in" 'delete-other-windows 'max :help "maximise window")
;; utils
(tool-bar-add-item "sort-column-ascending" 'diary 'diary :help "display diary")
(tool-bar-add-item "sort-descending" 'fleet/todo-visit 'visit :help "visit todo")
(tool-bar-add-item "lock-ok" 'org-agenda-list 'habit :help "habit")
(tool-bar-add-item "separator" nil 'Nil)
(tool-bar-add-item "describe" 'newsticker-show-news 'news :help "News ticker")
(tool-bar-add-item "next-page" 'eww-list-bookmarks 'eww-bookmark :help "EWW bookmark")
(tool-bar-add-item "separator" nil 'Nil2)
(tool-bar-add-item "sort-criteria" 'fleet/todo-org 'todo :help "new todo")
(tool-bar-add-item "info" 'fleet/done-org 'done :help "done todo")
(tool-bar-add-item "separator" nil 'Nil3)
;; (tool-bar-add-item "up-arrow" 'set-mark-command 'setmark :help "set mark")
;; (tool-bar-add-item "sort-ascending" 'fleet/add-region 'add-region :help "add region to fleet")
(tool-bar-add-item "jump-to" 'glossary/add-at-point 'add-to-glossary :help "add to glossary")
(tool-bar-add-item "spell" 'glossary/revisit 'glossary :help "revisit glossary")
(tool-bar-local-item "next-page" 'eww-list-bookmarks 'eww-bookmark eww-tool-bar-map :help "EWW bookmark")
(tool-bar-local-item "up-arrow" 'fleet/mark-region 'fleet/mark-region eww-tool-bar-map)
(tool-bar-local-item "sort-ascending" 'fleet/add-region 'fleet/add-region eww-tool-bar-map)
(tool-bar-local-item "help" 'stardict-define-at-point 'stardict-define-at-point eww-tool-bar-map)
(tool-bar-local-item "checked" 'eww-readable 'eww-readable eww-tool-bar-map)

;; mode line
(setq-default mode-line-format
      '("%e" mode-line-front-space mode-line-modes " " mode-line-buffer-identification " " mode-line-misc-info " " mode-line-position
  mode-line-end-spaces))

;; menu
(define-key global-map
  [menu-bar edit fleet/add-region]
  '("copy region to fleet" . fleet/add-region))
(define-key global-map
  [menu-bar edit set-mark]
  '("mark region" . fleet/mark-region))
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
  [menu-bar my @300-visit-tangshi-file]
  '("visit tangshi file" . @300-visit-tangshi-file))
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
;; (define-key global-map
;;   [menu-bar my fleet/add-region]
;;   '("copy region to fleet note" . fleet/add-region))
(define-key global-map
  [menu-bar my fleet/add-url]
  '("copy URL to fleet note" . fleet/add-url))
(define-key global-map
  [menu-bar my webjump]
  '("web jump" . webjump))
;; (define-key global-map
;;   [menu-bar my eww-readable]
;;   '("EWW readable" . eww-readable))
;; (define-key global-map
;;   [menu-bar my eww-list-bookmarks]
;;   '("EWW bookmark" . eww-list-bookmarks))
(define-key global-map
  [menu-bar my eww]
  '("EWW" . eww))
; dictionary
;; (define-key global-map
;;   [menu-bar my glossary/add-at-point]
;;   '("Add to glossary" . glossary/add-at-point))
(define-key global-map
  [menu-bar my glossary/flow]
  '("Define word cont." . glossary/flow))
(define-key global-map
  [menu-bar my stardict-define]
  '("Define word" . stardict-define))
(define-key global-map
  [menu-bar my stardict-define-at-point]
  '("Define at point" . stardict-define-at-point))
;; habit
(define-key global-map
  [menu-bar my habit/org-habit-done]
  '("Complete habit" . habit/org-habit-done))
(define-key global-map
  [menu-bar my habit/visit-habit-file]
  '("Visit habit file" . habit/visit-habit-file))
;; (define-key global-map
;;   [menu-bar my org-agenda-list]
;;   '("List habit" . org-agenda-list))

;; third-party packages
(require 'pangu-spacing)
(add-hook 'org-mode-hook #'pangu-spacing-mode)

(require '@300)
(defun @300-parse-to-json (file)
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char (point-min))
      (let ((poems nil)
            (counter 0))
        (while (< (point) (point-max))
          (let ((poem (list `(id . ,counter) '(type . ""))))
            ;; title
            (push `(title . ,(buffer-substring (line-beginning-position) (line-end-position))) poem)
            (forward-line 1)
            ;; author
            (push `(author . ,(buffer-substring (line-beginning-position) (line-end-position))) poem)
            (forward-line 1)
            ;; verses
            (let ((verses nil))
              (while-let ((line (buffer-substring (line-beginning-position) (line-end-position)))
                          (empty-p (not (string= line ""))))
                (push line verses)
                (forward-line 1))
              (push `(contents . ,(mapconcat #'identity (reverse verses) "\n")) poem)
              ;; skip the empty line
              (forward-line 1))
            (push poem poems)
            (setq counter (1+ counter))))
        (let ((output-str (json-encode poems))
              (output-file (concat (buffer-file-name) ".json")))
          (with-current-buffer (find-file-noselect output-file)
            (erase-buffer)
            (insert output-str)
            (json-pretty-print-buffer)
            (save-buffer)
            (kill-buffer)))))
    (kill-buffer)))
(@300-parse-to-json (concat emacs-dir "tangshi.org"))
(setq @300-json (concat emacs-dir "tangshi.org.json"))
(tool-bar-add-item "spell"
                   (lambda () (interactive) (@300-random) (switch-to-buffer "*唐诗三百首*") (delete-other-windows))
                   'random-shi :help "random tangshi")
(defun @300-visit-tangshi-file ()
  (interactive)
  (switch-to-buffer (find-file-noselect (concat emacs-dir "tangshi.org")))
  (goto-char (point-max)))

;; finalise startup apperance
(with-current-buffer "*scratch*"
  (org-mode))
(switch-to-buffer "*Fancy Diary Entries*") ; generated by appt
(delete-other-windows)
