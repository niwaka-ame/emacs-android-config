(defvar emacs-dir "/sdcard/emacs/kawa/")

;; load theme
;; (add-to-list 'custom-theme-load-path "/sdcard/emacs/emacs-android-config/eink-emacs/")
;; (load-theme 'eink t)
(setopt tool-bar-position 'bottom)

;;; larger font on phone
(set-face-attribute 'default nil :height 230)
(require 'face-remap)
(setq text-scale-mode-step 1.1)
(setq pop-up-windows nil)
(setq auto-save-default nil)

;;; smaller font in mode line (such that at least part of the buffer name is displayed)
(set-face-attribute 'mode-line nil :height 0.8)

;;; set up a splash screen for diary

(setq inhibit-startup-screen t
      initial-scratch-message nil)

(add-to-list 'load-path "/sdcard/emacs/emacs-android-config/")
(let ((default-directory "/sdcard/emacs/emacs-android-config/"))
  (normal-top-level-add-subdirs-to-load-path))

(setq make-backup-files nil)

;; some useful builtin mode
(require 'dired)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(setq dired-kill-when-opening-new-dired-buffer t)
(advice-add 'dired-mouse-find-file-other-window :override 'dired-mouse-find-file)

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
;; (require 'webjump)
(require 'eww)
(setq browse-url-browser-function 'eww)
;; (setq webjump-sites
;;       '(("ddg" . [simple-query "duckduckgo.com" "duckduckgo.com/?q=" ""])
;;         ("gsc" . [simple-query "scholar.google.com" "scholar.google.com/scholar?q=" ""])))
(setq eww-bookmarks-directory emacs-dir
      shr-inhibit-images t)

;; eww-mode
(tool-bar-local-item "next-page" 'eww-list-bookmarks 'eww-bookmark eww-tool-bar-map)
;; (tool-bar-local-item "sort-ascending" 'fleet/add-region 'fleet/add-region eww-tool-bar-map)
(tool-bar-local-item "copy" 'copy-region-as-kill 'copy-region-as-kill eww-tool-bar-map)
(tool-bar-local-item "help" 'stardict-define-at-point 'stardict-define-at-point eww-tool-bar-map)
(tool-bar-local-item "connect-to-url" 'gptel/ask-llama 'gptel/ask-llama eww-tool-bar-map)
(tool-bar-local-item "checked" 'eww-readable 'eww-readable eww-tool-bar-map)

(defvar eww-bookmark-tool-bar-map
  (let ((tool-bar-map (make-sparse-keymap)))
    (tool-bar-add-item "close" 'kill-current-buffer 'kill-current-buffer)
    (tool-bar-add-item "open" 'eww-bookmark-browse 'browse)
    tool-bar-map))
(add-hook 'eww-bookmark-mode-hook (lambda () (setq-local tool-bar-map eww-bookmark-tool-bar-map)))


;; truncate line in orgmode
(require 'org)
(add-hook 'org-mode-hook (lambda () (toggle-truncate-lines -1)))
(add-hook 'org-mode-hook #'variable-pitch-mode)
(add-hook 'org-mode-hook #'org-indent-mode)
(add-hook 'org-mode-hook #'delete-other-windows)
;; larger font size
(add-hook 'org-mode-hook (lambda () (text-scale-set 1)))
(setq org-link-frame-setup
      '((vm . vm-visit-folder-other-frame)
        (vm-imap . vm-visit-imap-folder-other-frame)
        (gnus . org-gnus-no-new-news)
        (file . find-file)
        (wl . wl-other-frame)))


;; org-habit
(define-derived-mode habit-mode org-mode "habit")

(defvar habit-tool-bar-map
  (let ((tool-bar-map (make-sparse-keymap)))
    (tool-bar-add-item "close" 'kill-current-buffer 'kill-current-buffer)
    (tool-bar-add-item "undo" 'undo 'undo)
    (tool-bar-add-item "save" 'save-buffer 'save)
    (tool-bar-add-item "sort-criteria" 'habit/add-habit 'add)
    (tool-bar-add-item "info" 'habit/org-habit-done 'complete)
    (tool-bar-add-item "lock-ok" 'habit/visit-habit-file 'habit)
    tool-bar-map))

(add-hook 'habit-mode-hook (lambda () (setq-local tool-bar-map habit-tool-bar-map)))

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
  (if (string= (buffer-name) "habits.org")
      (org-agenda-list)
    (progn
      (switch-to-buffer (find-file-noselect (concat emacs-dir "habits.org")))
      (habit-mode)
      (org-cycle-content))))

(defun habit/add-habit (habit freq)
  (interactive "sHabit: \nsFrequency: ")
  (with-current-buffer "habits.org"
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
(define-derived-mode fleet-mode org-mode "fleet")

(defvar fleet-tool-bar-map
  (let ((tool-bar-map (make-sparse-keymap)))
    (tool-bar-add-item "close" 'kill-current-buffer 'kill-current-buffer)
    (tool-bar-add-item "undo" 'undo 'undo)
    (tool-bar-add-item "save" 'save-buffer 'save)
    (tool-bar-add-item "sort-criteria" 'fleet/todo-org 'todo)
    (tool-bar-add-item "info" 'fleet/done-org 'done)
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
             (url (and (string= (buffer-name) "*eww*") (plist-get eww-data :url))))
        (copy-region-as-kill beg (1+ end))
        (setq fleet/region nil)
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

(defun lit/add-region ()
  (interactive)
  (if (region-active-p)
      (let* ((beg (region-beginning))
             (end (region-end))
             (buf (current-buffer))
             (buf-name (substring (buffer-name) 0 (- (length (buffer-name)) 5))))
        (copy-region-as-kill beg (1+ end))
        (setq fleet/region nil)
        (with-current-buffer
            (find-file-noselect (concat emacs-dir "bib-notes/" buf-name ".org"))
          (goto-char (point-max))
          (org-insert-heading)
          (let ((string-to-add (replace-regexp-in-string "\n\n" "\t" (car kill-ring))))
            ;; use \\t to represent new paragraph
            (if (string-match-p "[\u4e00-\u9fff]" string-to-add)
                (progn
                  (insert (replace-regexp-in-string "\n" "" string-to-add)))
              (progn
                (insert (replace-regexp-in-string "\n" " " string-to-add)))))
          (newline)
          (save-buffer)))
    (message "mark region first!")))

(defun lit/visit-note ()
  (interactive)
  (switch-to-buffer
   (find-file-noselect
    (concat emacs-dir "bib-notes/" (substring (buffer-name) 0 (- (length (buffer-name)) 5)) ".org")))
  (lit-mode))

(require 'nov-grep)
(defun lit/visit-epub ()
  (interactive)
  (let* ((heading (org-get-heading t t t t))
         (first-line (car (split-string heading "\t")))
         (epub-buffer (concat (substring (buffer-name) 0 (- (length (buffer-name)) 3)) "epub")))
    (switch-to-buffer epub-buffer)
    (my-nov-grep first-line)))

;; use org files for literature notes
(define-derived-mode lit-mode org-mode "lit")
(defvar lit-tool-bar-map
  (let ((tool-bar-map (make-sparse-keymap)))
    (tool-bar-add-item "close" 'kill-current-buffer 'kill-current-buffer)
    (tool-bar-add-item "undo" 'undo 'undo)
    (tool-bar-add-item "save" 'save-buffer 'save)
    (tool-bar-add-item "search" 'lit/visit-epub 'lit/visit-epub)
    tool-bar-map))
(add-hook 'lit-mode-hook (lambda () (setq-local tool-bar-map lit-tool-bar-map)))

(defun fleet/add-url ()
  (interactive)
  (let ((url (plist-get eww-data :url)))
    (when url
      (fleet/todo-org 'no-switch)
      (with-current-buffer "todo.org"
        (insert url)
        (save-buffer)))))

(defun visit-books ()
  (interactive)
  (find-file-noselect "/sdcard/emacs/books/")
  (switch-to-buffer "books")
  (dired-revert))

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

(defvar glossary/hide-p t)

(defun glossary/revisit ()
  (interactive)
  (if glossary/hide-p
      (progn
        (glossary/revisit-load)
        (setq glossary/hide-p nil))
    (progn
      (switch-to-buffer "*glossary-revisit*")
      (set-text-properties (point-min) (point-max) nil)
      (setq glossary/hide-p t))))

(defun glossary/revisit-load ()
  (let ((word-number 4))
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
                (let* ((word-def (stardict--lookup-and-return word))
                       (by-line (split-string word-def "\n"))
                       (shown (mapconcat 'identity (cl-subseq by-line 0 2) "\n"))
                       (hidden (mapconcat 'identity (cl-subseq by-line 2) "\n")))
                  (insert shown)
                  (newline)
                  (insert (propertize hidden 'face '(:foreground "white"))))
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

;; define a local tool bar for stardict
(defvar stardict-tool-bar-map
  (let ((tool-bar-map (make-sparse-keymap)))
    (tool-bar-add-item "close" 'kill-current-buffer 'kill-current-buffer)
    (tool-bar-add-item "help" 'stardict-define-at-point 'stardict-define-at-point)
    (tool-bar-add-item "jump-to" 'glossary/add-at-point 'add-to-glossary)
    tool-bar-map))

(add-hook 'stardict-mode-hook (lambda () (setq-local tool-bar-map stardict-tool-bar-map)))

;; third-party packages
(require 'pangu-spacing)
(add-hook 'org-mode-hook #'pangu-spacing-mode)

(require 'nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(define-key nov-mode-map (kbd "<volume-up>") 'nov-scroll-down)
(define-key nov-mode-map (kbd "<volume-down>") 'nov-scroll-up)
(defvar nov-tool-bar-map
  (let ((tool-bar-map (make-sparse-keymap)))
    (tool-bar-add-item "close" 'kill-current-buffer 'kill-current-buffer)
    (tool-bar-add-item "open" 'visit-books 'nov-open)
    (tool-bar-add-item "search" 'my-nov-grep 'my-nov-grep)
    (tool-bar-add-item "home" 'nov-goto-toc 'nov-goto-toc)
    (tool-bar-add-item "left-arrow" 'nov-previous-document 'nov-previous-document)
    (tool-bar-add-item "right-arrow" 'nov-next-document 'nov-next-document)
    (tool-bar-add-item "sort-ascending" 'lit/add-region 'lit/add-region)
    (tool-bar-add-item "copy" 'copy-region-as-kill 'copy-region-as-kill)
    (tool-bar-add-item "help" 'stardict-define-at-point 'stardict-define-at-point)
    (tool-bar-add-item "connect-to-url" 'gptel/ask-llama 'gptel/ask-llama)
    ;; (tool-bar-add-item "zoom-in" 'delete-other-windows 'max)
    (tool-bar-add-item "exit" 'lit/visit-note 'switch)
    tool-bar-map))
(add-hook 'nov-mode-hook (lambda () (setq-local tool-bar-map nov-tool-bar-map)))

(require '@300)
(defun @300/parse-to-json (file)
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
(@300/parse-to-json (concat emacs-dir "tangshi.org"))
(setq @300-json (concat emacs-dir "tangshi.org.json"))
(defvar @300/prose-hidden 0)

(defun @300/visit-tangshi-file ()
  (interactive)
  (switch-to-buffer (find-file-noselect (concat emacs-dir "tangshi.org")))
  (goto-char (point-max)))

(defun @300/random-shi ()
  (interactive)
  (if (= @300/prose-hidden 0)
      (progn
        (@300-random)
        (switch-to-buffer "*唐诗三百首*")
        (text-scale-set 2)
        (@300/hide-prose))
    (@300/show-prose))
  (delete-other-windows)
  )

(defun @300/hide-prose ()
  (with-current-buffer "*唐诗三百首*"
    (save-excursion
      (goto-line 4)
      (while (< (point) (point-max))
        (beginning-of-line)
        (let* ((rand (random 3))
               (line-end (line-end-position))
               (line-beg (line-beginning-position))
               (comma-pos (re-search-forward "[，？。！]" line-end t)))
          (when comma-pos
            (if (= rand 1)
                (put-text-property comma-pos line-end 'face '(:foreground "white"))
              (put-text-property line-beg comma-pos 'face '(:foreground "white"))))
          (forward-line 1)
          ))))
  (setq @300/prose-hidden 1))


(defun @300/show-prose ()
  (with-current-buffer "*唐诗三百首*"
    (remove-text-properties (point-min) (point-max) '(face nil))
    (setq @300/prose-hidden 0)))


(require 'elfeed)
(require 'elfeed-org)
(elfeed-org)
(setq elfeed-db-directory (concat emacs-dir ".elfeed-data"))
(setq rmh-elfeed-org-files (list (concat emacs-dir "elfeed.org")))
(setq elfeed-show-entry-switch 'switch-to-buffer)
(setq elfeed-search-date-format (list "%y%m%d" 6 :left))
(define-key elfeed-search-mode-map [mouse-1] 'elfeed-search-show-entry)
(define-key elfeed-show-mode-map (kbd "<volume-up>") 'elfeed-show-prev)
(define-key elfeed-show-mode-map (kbd "<volume-down>") 'elfeed-show-next)

(defvar elfeed-tool-bar-map
  (let ((tool-bar-map (make-sparse-keymap)))
    (tool-bar-add-item "close" 'kill-current-buffer 'kill-current-buffer)
    (tool-bar-add-item "refresh" 'elfeed-update 'elfeed-update)
    (tool-bar-add-item "help" 'stardict-define-at-point 'stardict-define-at-point)
    (tool-bar-add-item "connect-to-url" 'gptel/ask-llama 'gptel/ask-llama)
    tool-bar-map))
(add-hook 'elfeed-search-mode-hook (lambda () (setq-local tool-bar-map elfeed-tool-bar-map)))
(add-hook 'elfeed-show-mode-hook (lambda () (setq-local tool-bar-map elfeed-tool-bar-map)))

(require 'org-journal)
(setq org-journal-dir (concat emacs-dir "journal/")
      org-journal-date-format "%Y-%m-%d, %A"
      org-journal-file-format "%Y-%V-%b.org"
      org-journal-carryover-items ""
      org-journal-file-type 'weekly
      org-journal-encrypt-journal nil
      org-journal-hide-entries-p nil)

(require 'markdown-mode)
(require 'gptel)
(require 'gptel-curl)
(require 'gptel-transient)
(load (concat emacs-dir "llama-directives.el"))
(setq gptel--system-message (alist-get 'default gptel-directives))
;; OpenRouter offers an OpenAI compatible API
(setq gptel-model "meta-llama/llama-3.1-8b-instruct:free"
      gptel-max-tokens 500
      gptel-backend
      (gptel-make-openai "OpenRouter"               ;Any name you want
                         :host "openrouter.ai"
                         :endpoint "/api/v1/chat/completions"
                         :stream t
                         :key (with-current-buffer (find-file-noselect (concat emacs-dir "llama")) (buffer-substring-no-properties (point-min) (1- (point-max))))
                         :models '("meta-llama/llama-3.1-8b-instruct:free")))
(add-hook 'markdown-mode-hook #'variable-pitch-mode)

(defun gptel/start-or-send ()
  (interactive)
  (if (string= major-mode "markdown-mode")
      (if (string= (buffer-substring-no-properties (- (point-max) 4) (1- (point-max))) "###")
          (message "empty input!")
        (gptel-send))
    (switch-to-buffer (gptel "*Llama3*"))))

(defun gptel/ask-llama ()
  (interactive)
  (let ((query nil)
        (sentence (replace-regexp-in-string "\n" " " (thing-at-point 'sentence))))
    (if (region-active-p)
        (setq query (replace-regexp-in-string
                     "\n"
                     " "
                     (buffer-substring-no-properties (region-beginning)
                                                     (region-end))))
      (setq query (thing-at-point 'word)))
    (switch-to-buffer (gptel "*Ask Llama3*"))
    (erase-buffer)
    (insert "### What is \"" query "\" in the context of \"" sentence "\"?")
    (gptel-send)))

(defvar org-journal-tool-bar-map
  (let ((tool-bar-map (make-sparse-keymap)))
    (tool-bar-add-item "close" 'kill-current-buffer 'kill-current-buffer)
    (tool-bar-add-item "open"
                       (lambda ()
                         (interactive)
                         (find-file-noselect org-journal-dir)
                         (switch-to-buffer "journal"))
                       'open-org-journal-dir)
    (tool-bar-add-item "undo" 'undo 'undo)
    (tool-bar-add-item "save" 'save-buffer 'save)
    (tool-bar-add-item "copy" 'copy-region-as-kill 'copy-region-as-kill)
    (tool-bar-add-item "paste" 'yank 'yank)
    (tool-bar-add-item "left-arrow" 'org-journal-previous-entry 'org-journal-previous-entry)
    (tool-bar-add-item "right-arrow" 'org-journal-next-entry 'org-journal-next-entry)
    (tool-bar-add-item "search-replace"
                       (lambda (prefix) (interactive "P") (org-journal-new-entry prefix) (delete-other-windows))
                       'org-journal-new-entry)
    tool-bar-map))
(add-hook 'org-journal-mode-hook (lambda () (setq-local tool-bar-map org-journal-tool-bar-map)))
(add-hook 'org-journal-mode-hook #'delete-other-windows)

(require 'denote)
(setq denote-directory (concat emacs-dir "notes/")
      denote-backlinks-show-context t)
(setq denote-known-keywords
      '(meta idea nonfiction fiction film documentary other))
(defun denote/visit-denote-dir ()
  (interactive)
  (with-current-buffer (find-file-noselect denote-directory)
    (switch-to-buffer (current-buffer))))

;; tool bar
;; (tool-bar-add-item "home" 'execute-extended-command 'Mx :help "execute command")
;; (tool-bar-add-item "zoom-in" 'text-scale-increase 'zoom-in)
;; (tool-bar-add-item "close" 'delete-window 'delete-window)
(tool-bar-add-item "connect-to-url" 'gptel/start-or-send 'gptel/start-or-send)
;; utils
(tool-bar-add-item "sort-column-ascending" 'diary 'diary)
(tool-bar-add-item "sort-descending" 'fleet/todo-visit 'visit)
(tool-bar-add-item "lock-ok" 'habit/visit-habit-file 'habit)
(tool-bar-add-item "next-page" 'eww-list-bookmarks 'eww-bookmark)
(tool-bar-add-item "spell" 'glossary/revisit 'glossary)
(tool-bar-add-item "spell"
                   '@300/random-shi
                   'random-shi :help "random tangshi")
(tool-bar-add-item "next-page" 'visit-books 'visit-books)
(tool-bar-add-item "describe" 'elfeed 'elfeed)
(tool-bar-add-item "spell" 'denote/visit-denote-dir 'denote)
(tool-bar-add-item "search-replace"
                   (lambda (prefix) (interactive "P") (org-journal-new-entry prefix) (delete-other-windows))
                   'org-journal-new-entry)

;; mode line
(setq-default mode-line-format
      '("%e" mode-line-front-space mode-line-modes " " mode-line-position " " mode-line-buffer-identification " " mode-line-misc-info
  mode-line-end-spaces))
(setq-default line-number-mode nil)

;; menu
(define-key global-map
  [menu-bar edit fleet/add-region]
  '("copy region to fleet" . fleet/add-region))
(define-key global-map
  [menu-bar edit copy-region-as-kill]
  '("copy region" . copy-region-as-kill))
; remove two menus
(define-key global-map [menu-bar options] nil)
(define-key global-map [menu-bar tools] nil)
; add my own menu
(define-key-after
  (lookup-key global-map [menu-bar])
  [my]
  (cons "My-magic" (make-sparse-keymap "My-magic"))
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
  [menu-bar my @300/visit-tangshi-file]
  '("visit tangshi file" . @300/visit-tangshi-file))
(define-key global-map
  [menu-bar my visit-kawa]
  '("visit kawa dir" .
    (lambda ()
      (interactive)
      (find-file-noselect emacs-dir)
      (switch-to-buffer "kawa"))))
(define-key global-map
  [menu-bar my visit-config]
  '("visit config dir" .
    (lambda ()
      (interactive)
      (find-file-noselect "/sdcard/emacs/emacs-android-config/")
      (switch-to-buffer "emacs-android-config"))))
(define-key global-map
  [menu-bar my fleet/add-url]
  '("copy URL to fleet note" . fleet/add-url))
;; (define-key global-map
;;   [menu-bar my webjump]
;;   '("web jump" . webjump))
(define-key global-map
  [menu-bar my eww]
  '("EWW" . eww))
(define-key global-map
  [menu-bar my glossary/flow]
  '("Define word cont." . glossary/flow))
(define-key global-map
  [menu-bar my stardict-define]
  '("Define word" . stardict-define))
(define-key global-map
  [menu-bar my stardict-define-at-point]
  '("Define at point" . stardict-define-at-point))
(define-key global-map
  [menu-bar my denote-backlinks]
  '("Denote backlinks" . denote-backlinks))
(global-set-key (kbd "<volume-up>") 'scroll-down-command)
(global-set-key (kbd "<volume-down>") 'scroll-up-command)

;; TAB for minibuffer
(define-key minibuffer-local-map (kbd "<volume-down>") 'minibuffer-complete)
(define-key minibuffer-local-completion-map (kbd "<volume-down>") 'minibuffer-complete)
(define-key minibuffer-local-completion-map (kbd "<volume-down>") 'minibuffer-complete)
(define-key minibuffer-local-filename-completion-map (kbd "<volume-down>") 'minibuffer-complete)
(define-key minibuffer-local-must-match-map (kbd "<volume-down>") 'minibuffer-complete)

;; finalise startup apperance
(add-hook 'emacs-startup-hook
          (lambda ()
            (switch-to-buffer "*Fancy Diary Entries*")))
