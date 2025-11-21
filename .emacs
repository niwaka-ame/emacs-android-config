(defvar emacs-dir "/sdcard/emacs/kawa/")
(defvar path-dir "/sdcard/emacs/emacs-android-config/")

;;; path
(add-to-list 'load-path path-dir)
(let ((default-directory path-dir))
  (normal-top-level-add-subdirs-to-load-path))

;;; load theme
(setq make-backup-files nil)

;;; appearance
(setopt tool-bar-position 'bottom)
;; larger font on phone
(set-face-attribute 'default nil :height 240)
(set-fontset-font t nil (font-spec :name "Nom Na Tong"))
(require 'face-remap)
(setq text-scale-mode-step 1.1)
(setq pop-up-windows nil)
(setq auto-save-default nil)
;; font
(set-face-attribute 'default nil :family "LXGW WenKai Screen")
(with-eval-after-load 'eww
  (set-face-attribute 'shr-text nil :family "LXGW WenKai Screen"))
;; toolbar buttons
;; load customised 24x24 xpm images
(add-to-list 'image-load-path (concat path-dir "images/"))
(set-face-attribute 'tool-bar nil :background "white smoke")
;; disable clicking on minibuffer
(define-key minibuffer-inactive-mode-map [mouse-1] 'ignore)
;; always display only one window on android
(when (string= system-type "android")
  (add-hook 'window-configuration-change-hook
            (lambda ()
              (when (> (length (window-list nil 'NO-MINIBUF)) 1)
                (delete-other-windows)))))

;;; mode line
;; smaller font in mode line (such that at least part of the buffer name is displayed)
;; also lighter background
(set-face-attribute 'mode-line nil :height 0.8 :background "gainsboro")
(setq-default mode-line-format
      '("%e" mode-line-front-space mode-line-modes " " mode-line-position " " mode-line-buffer-identification " " mode-line-misc-info
  mode-line-end-spaces))
(setq-default line-number-mode nil)

;;; some useful builtin mode
(require 'dired)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(setq dired-listing-switches "-alt --group-directories-first")
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

;;; shr
;; preventing shr from adding hard line break when rendering.
;; see https://emacs.stackexchange.com/questions/31882/how-to-prevent-eww-from-truncating-lines
;; requires for nov-grep to function normally
(eval-after-load 'shr
  '(progn (setq shr-width -1)
          (defun shr-fill-text (text) text)
          (defun shr-fill-lines (start end) nil)
          (defun shr-fill-line () nil)))

;;; EWW
(require 'browse-url)
(require 'eww)
(setq browse-url-browser-function 'eww)
(setq eww-bookmarks-directory emacs-dir
      shr-inhibit-images t)
;; add this hook to allow soft line break in EWW
(add-hook 'eww-after-render-hook #'visual-line-mode)

(tool-bar-local-item "search" 'isearch-forward 'search eww-tool-bar-map)
(tool-bar-local-item "star" 'eww-list-bookmarks 'EWW-bookmark eww-tool-bar-map)
;; (tool-bar-local-item "sort-ascending" 'fleet/add-region 'fleet/add-region eww-tool-bar-map)
(tool-bar-local-item "copy" 'copy-region-as-kill 'copy eww-tool-bar-map)
(tool-bar-local-item "dict" 'glossary/define-at-point 'dict eww-tool-bar-map)
(tool-bar-local-item "robot" 'gptel/ask-llama 'GPT eww-tool-bar-map)
; (tool-bar-local-item "contact" 'eww-toggle-images 'toggle-images eww-tool-bar-map)
(tool-bar-local-item "next-page" 'eww/eww-readable 'EWW-readable eww-tool-bar-map)

(defvar eww-bookmark-tool-bar-map
  (let ((tool-bar-map (make-sparse-keymap)))
    (tool-bar-add-item "close" 'kill-current-buffer 'close)
    (tool-bar-add-item "open" 'eww-bookmark-browse 'browse)
    tool-bar-map))
(add-hook 'eww-bookmark-mode-hook (lambda () (setq-local tool-bar-map eww-bookmark-tool-bar-map)))
(add-hook 'eww-bookmark-mode-hook (lambda () (text-scale-set 1)))

(defun eww/remove-long-dashes()
  (interactive)
  (when (eq major-mode 'eww-mode)
    (let ((inhibit-read-only t)) ; Temporarily allow modifications
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "-\\{11,\\}" nil t)
          (replace-match "====="))))))
(add-hook 'eww-after-render-hook #'eww/remove-long-dashes)
(add-hook 'eww-mode-hook (lambda () (text-scale-set -1)))

(defun eww/estimate-read-time ()
  (let* ((word-count (count-words (point-min) (point-max)))
         ;; readable mode won't remove all structural text
         (word-discount 0.95)
         ;; conservative estimation for long reads
         (word-per-min 150))
    (message "Estimated read time: %d min." (/ (* word-count word-discount) 150))))

(defun eww/eww-readable ()
  (interactive)
  (eww-readable)
  (eww/remove-long-dashes)
  (eww/estimate-read-time))

;;; org mode
(require 'org)
(setq org-extend-today-until 2)
(add-hook 'org-mode-hook (lambda () (toggle-truncate-lines -1)))
(add-hook 'org-mode-hook #'org-indent-mode)
(add-hook 'org-mode-hook (lambda () (set-face-attribute 'org-level-1 nil :weight 'bold)))
(setq org-link-frame-setup
      '((vm . vm-visit-folder-other-frame)
        (vm-imap . vm-visit-imap-folder-other-frame)
        (gnus . org-gnus-no-new-news)
        (file . find-file)
        (wl . wl-other-frame)))


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

;;; fleeting notes
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

;;; literature notes
(defun hlt/add-region ()
  (interactive)
  (if (region-active-p)
      (let* ((beg (region-beginning))
             (end (region-end))
             (buf (current-buffer))
             (buf-name (substring (buffer-name) 0 (- (length (buffer-name)) 5)))
             (novindex nov-documents-index))
        (copy-region-as-kill beg (1+ end))
        (with-current-buffer
            (find-file-noselect (concat emacs-dir "hl-notes/" buf-name ".org"))
          (goto-char (point-max))
          (when (not (= (point-min) (point-max))) (newline))
          (org-insert-heading)
          (org-set-property "NOVINDEX" (number-to-string novindex))
          (let ((string-to-add (string-replace "\n" "\t" (car kill-ring))))
            ;; use \\t to represent new line
            (insert string-to-add))
          (save-buffer)))
    (message "mark region first!")))

(defun hlt/visit-note ()
  (interactive)
  (switch-to-buffer
   (find-file-noselect
    (concat emacs-dir "hl-notes/" (substring (buffer-name) 0 (- (length (buffer-name)) 5)) ".org")))
  (hlt-mode))

(defun hlt/visit-epub ()
  (interactive)
  (let* ((heading (org-get-heading t t t t))
         ;; search appears struggling with these quotation marks.
         (first-line (replace-regexp-in-string "[“”]" "." (car (split-string heading "\t"))))
         (epub-buffer (concat (substring (buffer-name) 0 (- (length (car (split-string (buffer-name) "<"))) 3)) "epub"))
         (novindex (org-entry-get (point) "NOVINDEX")))
    (switch-to-buffer epub-buffer)
    (nov-goto-document (string-to-number novindex))
    ;; must unhighlight first, otherwise the highlight won't work when revisit.
    (unhighlight-regexp first-line)
    (highlight-regexp first-line)
    (re-search-forward first-line)))

(define-derived-mode hlt-mode org-mode "hlt")
(defvar hlt-tool-bar-map
  (let ((tool-bar-map (make-sparse-keymap)))
    (tool-bar-add-item "close" 'kill-current-buffer 'close)
    (tool-bar-add-item "undo" 'undo 'undo)
    (tool-bar-add-item "save" 'save-buffer 'save)
    (tool-bar-add-item "next-page" 'hlt/visit-epub 'jump-back)
    tool-bar-map))
(add-hook 'hlt-mode-hook (lambda () (setq-local tool-bar-map hlt-tool-bar-map)))

;;; glossary and stardict
(require 'stardict)
(require 'stardict-es-en)
(defvar glossary/context nil)
(defvar glossary/current-word nil)

(defun glossary/add-at-point ()
  (interactive)
  (let ((word (thing-at-point 'word)))
    (with-current-buffer (find-file-noselect (concat emacs-dir "glossary"))
      (goto-char (point-min))
      (if (and glossary/context
               glossary/current-word
               (string-match-p glossary/current-word glossary/context))
          (insert (concat word "\t" glossary/context "\n"))
        (insert (concat word "\n")))
      (setq glossary/context nil)
      (setq glossary/current-word nil)
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
      ;; pass the first line (which is not hidden) or an empty line
      (forward-line 1)
      (while (and (not (eobp)) (not (looking-at-p "^[[:space:]]*$")))
        ;; loop until reaches the next empty line
        (set-text-properties (line-beginning-position) (line-end-position) nil)
        (forward-line 1))
      (when (eobp)
        ;; revisit complete, go to next batch of words
        (set-text-properties (line-beginning-position) (line-end-position) nil)
        (setq glossary/hide-p t)))))

(defun glossary/revisit-load ()
  (let ((word-number 4))
    (with-current-buffer (find-file-noselect (concat emacs-dir "glossary"))
      (let ((total-word-number
             (count-lines (point-min) (point-max))))
        (dotimes (i word-number)
          (goto-char (point-min))
          (let ((line (random total-word-number)))
            (forward-line line)
            (let* ((word (downcase (thing-at-point 'word)))
                   (line (thing-at-point 'line))
                   (context (and (string-match-p "\t" line)
                                 (cadr (split-string line "\t")))))
              (with-current-buffer (get-buffer-create "*glossary-revisit*")
                (when (= i 0) (erase-buffer))
                (goto-char (point-max))
                (when (> i 0) (newline))
                (let* ((word-def (stardict--lookup-and-return word))
                       (by-line (split-string word-def "\n+"))
                       (shown (mapconcat 'identity (cl-subseq by-line 0 2) "\n"))
                       (hidden (mapconcat 'identity (cl-subseq by-line 2) "\n")))
                  (insert shown)
                  (newline)
                  (when context
                    (insert (replace-regexp-in-string word "~" context)))
                  (insert (propertize hidden 'face `(:foreground ,(face-attribute 'default :background)))))
                (newline))))))))
  (switch-to-buffer "*glossary-revisit*")
  (goto-char (point-min)))

(defun glossary/define-at-point ()
  (interactive)
  (unless (eq major-mode 'stardict-mode)
    (let ((context (thing-at-point 'sentence))
          (curr-word (thing-at-point 'word)))
      ;; in case we're not in a sentence but some e.g. bullet points
      (unless (string-match-p "\n" context)
        (setq glossary/context context)
        (setq glossary/current-word curr-word))))
  (stardict-define-at-point))

(defun glossary/flow (word)
  "Prompt for a word continuously."
  (interactive "sWord: ")
  (stardict--load-dict)
  (setq glossary/context nil)
  (setq glossary/current-word nil)
  (while (not (string= word "q")) ; "q" means quit
    (stardict-define word)
    (let ((str (read-string "Word: ")))
      (if (string= str "a") ; "a" means add to glossary
          (with-current-buffer (find-file-noselect (concat emacs-dir "glossary"))
            (goto-char (point-min))
            (insert (concat word "\n"))
            (save-buffer)
            (kill-buffer))
        ;; othewise update `WORD'
        (setq word str)))))

(defvar stardict-tool-bar-map
  (let ((tool-bar-map (make-sparse-keymap)))
    (tool-bar-add-item "close" 'kill-current-buffer 'close)
    (tool-bar-add-item "dict" 'glossary/define-at-point 'dict)
    (tool-bar-add-item "jump-to" 'glossary/add-at-point 'add-word)
    tool-bar-map))

(add-hook 'stardict-mode-hook (lambda () (setq-local tool-bar-map stardict-tool-bar-map)))

;;; pangu spacing
(require 'pangu-spacing)
(add-hook 'org-mode-hook #'pangu-spacing-mode)

;;; nov.el
(require 'init-nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(setq shr-max-width nil)
(setq shr-width 10000)
(add-hook 'nov-post-html-render-hook #'visual-line-mode)

(defun nov/visit-books ()
  (interactive)
  (find-file-noselect (concat emacs-dir "books/"))
  (switch-to-buffer "books")
  (dired-revert))

(define-key nov-mode-map (kbd "<volume-up>") 'nov-scroll-down)
(define-key nov-mode-map (kbd "<volume-down>") 'nov-scroll-up)
(defvar nov-tool-bar-map
  (let ((tool-bar-map (make-sparse-keymap)))
    (tool-bar-add-item "close" 'kill-current-buffer 'close)
    (tool-bar-add-item "open" 'nov/visit-books 'open)
    (tool-bar-add-item "copy" 'copy-region-as-kill 'copy)
    (tool-bar-add-item "search" 'isearch-forward 'search)
    (tool-bar-add-item "home" 'nov-goto-toc 'TOC)
    (tool-bar-add-item "left-arrow" 'nov-previous-document 'prev-chapter)
    (tool-bar-add-item "right-arrow" 'nov-next-document 'next-chapter)
    (tool-bar-add-item "plus" 'hlt/add-region 'highlight)
    (tool-bar-add-item "dict" 'glossary/define-at-point 'dict)
    (tool-bar-add-item "robot" 'gptel/ask-llama 'GPT)
    (tool-bar-add-item "journal" 'hlt/visit-note 'HL-note)
    (tool-bar-add-item "jump-to" 'nov-toggle-link-peeking 'peek)
    tool-bar-map))
(add-hook 'nov-mode-hook (lambda () (setq-local tool-bar-map nov-tool-bar-map)))
(add-hook 'nov-mode-hook #'visual-line-mode)
(add-hook 'nov-mode-hook (lambda () (text-scale-set -1)))

;;; poems
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

(defun @300/random-poem ()
  ;; status: 0 -> 1 -> ... -> N -> -1 -> 0
  (interactive)
  (if (= @300/prose-hidden 0)
      (progn
        (@300-random)
        (switch-to-buffer "*唐诗三百首*")
        (text-scale-set 2)
        (@300/hide-prose))
    (cond
     ((= @300/prose-hidden -1) (@300/show-prose-all))
     (t (@300/show-prose-one-by-one)))))

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
                (put-text-property comma-pos line-end 'face `(:foreground ,(face-attribute 'default :background)))
              (put-text-property line-beg comma-pos 'face `(:foreground ,(face-attribute 'default :background)))))
          (forward-line 1)
          ))))
  (setq @300/prose-hidden 1))


(defun @300/show-prose-all ()
  (switch-to-buffer "*唐诗三百首*")
  (with-current-buffer "*唐诗三百首*"
    (remove-text-properties (point-min) (point-max) '(face nil))
    (setq @300/prose-hidden 0)))

(defun @300/show-prose-one-by-one ()
  (switch-to-buffer "*唐诗三百首*")
  (with-current-buffer "*唐诗三百首*"
    (goto-line (+ @300/prose-hidden 3))
    (if (= (point) (point-max))
        (progn (@300/hide-prose) (setq @300/prose-hidden -1))
      (progn (remove-text-properties (line-beginning-position) (line-end-position) '(face nil))
             (cl-incf @300/prose-hidden)))))

;;; my timer
(defun my/timer (&optional seconds)
  "Display a timer from SECONDS in the echo area."
  (interactive (list (read-number "Timer seconds (default 90): " 90)))
  (if (> seconds 0)
      (progn
        (message "Timer: %d" seconds)
        (run-at-time 1 nil #'my/timer (1- seconds)))
    (message "Time's up!")))

;;; visit wikipedia
(defun my/visit-wikipedia (query)
  (interactive "sQuery: ")
  (let* ((query-trim (string-trim query))
         (query-no-space (replace-regexp-in-string "[ \t\n\r]+" "%20" query-trim)))
    (if (string-match-p "[\u4e00-\u9fff]" query-no-space)
        ;; query Chinese wikipedia
        (eww (concat "https://zh.m.wikipedia.org/w/index.php?search=" query-no-space))
      (eww (concat "https://en.m.wikipedia.org/w/index.php?search=" query-no-space)))))

;;; elfeed
(require 'elfeed)
(require 'elfeed-org)
(elfeed-org)
(setq elfeed-db-directory (concat emacs-dir ".elfeed-data"))
(setq rmh-elfeed-org-files (list (concat emacs-dir "elfeed.org")))
(setq elfeed-show-entry-switch 'switch-to-buffer)
(setq elfeed-search-date-format (list "%m%d" 4 :left))
(setq elfeed-search-title-min-width 200)
(setq elfeed-search-trailing-width 10)

(setq elfeed/filter-alist
      '((long . "@6-months-ago +long")
        (science . "@3-days-ago +sci")
        (academia . "@3-days-ago +aca")
        (all . "@6-months-ago")))
(setq elfeed-search-filter (cdar elfeed/filter-alist))

(defun elfeed-search-print-entry--notag (entry)
  "Print ENTRY to the buffer."
  (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
         (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
         (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
         (feed (elfeed-entry-feed entry))
         (feed-title
          (when feed
            (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
         (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
         (tags-str (mapconcat
                    (lambda (s) (propertize s 'face 'elfeed-search-tag-face))
                    tags ","))
         (title-width (- (window-width) 10 elfeed-search-trailing-width))
         (title-column (elfeed-format-column
                        title (elfeed-clamp
                               elfeed-search-title-min-width
                               title-width
                               elfeed-search-title-max-width)
                        :left)))
    (insert (propertize date 'face 'elfeed-search-date-face) " ")
    (when feed-title
      (insert (propertize feed-title 'face 'elfeed-search-feed-face) " "))
    (insert (propertize title-column 'face title-faces 'kbd-help title))))

(setq elfeed-search-print-entry-function #'elfeed-search-print-entry--notag)

(defun elfeed/set-preset-filter (filter)
  (elfeed-search-set-filter filter)
  (switch-to-buffer "*elfeed-search*"))

(defun elfeed/next ()
  (interactive)
  (if (eq (window-end nil t) (point-max))
      (elfeed-show-next)
    (scroll-up-command)))

(defun elfeed/prev ()
  (interactive)
  (if (eq (window-start) (point-min))
      (elfeed-show-prev)
    (scroll-down-command)))

(define-key elfeed-search-mode-map [mouse-1] 'elfeed-search-show-entry)
(define-key elfeed-show-mode-map (kbd "<volume-up>") 'elfeed/prev)
(define-key elfeed-show-mode-map (kbd "<volume-down>") 'elfeed/next)

(defun elfeed/menu-setup (alist)
  (let ((result (list "elfeed")))
    (dolist (pair alist result)
      (push `[,(symbol-name (car pair)) (lambda () (interactive) (elfeed-search-set-filter ,(cdr pair))) t] result))
    (reverse result)))
(easy-menu-define elfeed-search-mode-menu elfeed-search-mode-map
  "elfeed search mode menu"
  (elfeed/menu-setup elfeed/filter-alist))
(easy-menu-add elfeed-search-mode-menu elfeed-search-mode-map)

(defvar elfeed-tool-bar-map
  (let ((tool-bar-map (make-sparse-keymap)))
    (tool-bar-add-item "close" 'kill-current-buffer 'close)
    (tool-bar-add-item "refresh" 'elfeed-update 'update)
    (tool-bar-add-item "dict" 'glossary/define-at-point 'dict)
    (tool-bar-add-item "robot" 'gptel/ask-llama 'GPT)
    (dolist (pair elfeed/filter-alist)
        (tool-bar-add-item (symbol-name (car pair))
                           (eval `(lambda () (interactive) (elfeed/set-preset-filter ,(cdr pair))))
                           (car pair)))
    tool-bar-map))
(add-hook 'elfeed-search-mode-hook (lambda () (setq-local tool-bar-map elfeed-tool-bar-map)))
(add-hook 'elfeed-search-mode-hook (lambda () (text-scale-set -3)))
(add-hook 'elfeed-show-mode-hook (lambda () (setq-local tool-bar-map elfeed-tool-bar-map)))
(add-hook 'elfeed-show-mode-hook #'visual-line-mode)
(add-hook 'elfeed-show-mode-hook (lambda () (text-scale-set -1)))

;;; org-journal
(require 'org-journal)
(setq org-journal-dir (concat emacs-dir "journal/")
      org-journal-date-format "%Y-%m-%d, %A"
      org-journal-file-format "%Y-%V-%b.org"
      org-journal-carryover-items ""
      org-journal-file-type 'weekly
      org-journal-encrypt-journal nil
      org-journal-hide-entries-p nil)

(defun org-journal/new-entry (prefix)
  (interactive "P")
  (org-journal-new-entry prefix))

(defun org-journal/add-region (prefix)
  (interactive "P")
  (if (region-active-p)
      (let* ((beg (region-beginning))
             (end (region-end))
             (buf (current-buffer))
             (url (and (eq major-mode 'eww-mode) (plist-get eww-data :url)))
             (book (and (eq major-mode 'nov-mode) (buffer-name))))
        (copy-region-as-kill beg (1+ end))
        (org-journal-new-entry prefix)
        (save-excursion
          (newline)
          (if url (progn
                    (insert "From: ")
                    (insert url)
                    (newline)))
          (if book (progn
                    (insert "From: ")
                    (insert book)
                    (newline)))
          (insert "#+BEGIN_QUOTE\n")
          (insert (string-trim-right (car kill-ring)))
          (newline)
          (insert "#+END_QUOTE\n")
          (newline))))
  (message "mark region first!"))

;; use grep to search org-journal
(require 'grep)
(defun org-journal/search (pattern)
  (interactive "sPattern: ")
  (grep (concat "grep --color=auto -niH -e " pattern " *.org")))

(defvar org-journal-tool-bar-map
  (let ((tool-bar-map (make-sparse-keymap)))
    (tool-bar-add-item "close" 'kill-current-buffer 'close)
    (tool-bar-add-item "open"
                       (lambda ()
                         (interactive)
                         (find-file-noselect org-journal-dir)
                         (switch-to-buffer "journal"))
                       'open)
    (tool-bar-add-item "undo" 'undo 'undo)
    (tool-bar-add-item "save" 'save-buffer 'save)
    (tool-bar-add-item "search" 'org-journal/search 'search)
    (tool-bar-add-item "copy" 'copy-region-as-kill 'copy)
    (tool-bar-add-item "paste" 'yank 'paste)
    (tool-bar-add-item "left-arrow" 'org-journal-previous-entry 'previous-entry)
    (tool-bar-add-item "right-arrow" 'org-journal-next-entry 'next-entry)
    (tool-bar-add-item "journal"
                       'org-journal/new-entry
                       'new-entry)
    tool-bar-map))
(add-hook 'org-journal-mode-hook (lambda () (setq-local tool-bar-map org-journal-tool-bar-map)))
(remove-hook 'org-journal-mode-hook #'turn-on-visual-line-mode)

;;; llama3
(require 'markdown-mode)
(require 'gptel)
;; setq `gptel-directives'
(load (concat emacs-dir "llama-directives.el"))
;; make thing-at-point recognise sentence with single space.
(setq sentence-end-double-space nil)

;; OpenRouter offers an OpenAI compatible API
(setq gptel-model "tngtech/deepseek-r1t2-chimera:free"
      gptel-max-tokens 2000
      gptel-include-reasoning nil
      gptel-backend
      (gptel-make-openai "OpenRouter"               ;Any name you want
                         :host "openrouter.ai"
                         :endpoint "/api/v1/chat/completions"
                         :stream t
                         :key (with-current-buffer (find-file-noselect (concat emacs-dir "llama")) (buffer-substring-no-properties (point-min) (1- (point-max))))
                         :models '("tngtech/deepseek-r1t2-chimera:free")))
;; (add-hook 'markdown-mode-hook #'variable-pitch-mode)
(add-hook 'markdown-mode-hook (lambda () (setq gptel--system-message (alist-get 'default gptel-directives))))

;; define the gptel-mode menu according to `gptel-directives'
(defun gptel/menu-setup ()
  "loop over `gptel-directives' to generate a menu."
  (let ((result (list "gptel")))
    (dolist (pair gptel-directives result)
      (push `[,(symbol-name (car pair)) (lambda () (interactive) (setq-local gptel--system-message ,(cdr pair))) t] result))
    (reverse result)))
(easy-menu-define gptel-mode-menu gptel-mode-map
  "gptel mode menu"
  (gptel/menu-setup))
(easy-menu-add gptel-mode-menu gptel-mode-map)

(defun gptel/start-or-send ()
  (interactive)
  (if (eq major-mode 'markdown-mode)
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
    (if (string-match-p "[\u4e00-\u9fff]" query)
        (insert "### 在“" sentence "”里，“" query "”是什么意思？")
      (insert "### What is \"" query "\" in the context of \"" sentence "\"?"))
    (gptel-send)))

;;; tool bar
(tool-bar-add-item "diary" 'diary 'diary)
(tool-bar-add-item "routine" 'routine/visit-routine-file 'routine)
(tool-bar-add-item "earth" 'eww 'EWW)
(tool-bar-add-item "star" 'eww-list-bookmarks 'EWW-bookmark)
(tool-bar-add-item "wikipedia" 'my/visit-wikipedia 'wikipedia)
(tool-bar-add-item "rss" 'elfeed 'elfeed)
(tool-bar-add-item "spell" 'glossary/revisit 'glossary)
(tool-bar-add-item "poem" '@300/random-poem 'poems)
(tool-bar-add-item "timer" 'my/timer 'timer)
(tool-bar-add-item "nov" 'nov/visit-books 'books)
(tool-bar-add-item "journal"
                   'org-journal/new-entry
                   'journal)
(tool-bar-add-item "brain" 'roam/visit-zettel 'roam)
(tool-bar-add-item "robot" 'gptel/start-or-send 'GPT)
(tool-bar-add-item "todo" 'fleet/todo-visit 'todo)
(tool-bar-add-item "dict" 'glossary/flow 'dict)

;;; menu
; remove two menus
(define-key global-map [menu-bar options] nil)
(define-key global-map [menu-bar tools] nil)
; add my own menu
(define-key-after
  (lookup-key global-map [menu-bar])
  [my]
  (cons "魔术盒" (make-sparse-keymap "魔术盒"))
  'buffers)
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
  [menu-bar my glossary/flow]
  '("define word cont." . glossary/flow))
(define-key global-map
  [menu-bar my stardict-define]
  '("define word" . stardict-define))
(define-key global-map
  [menu-bar my glossary/define-at-point]
  '("define at point" . glossary/define-at-point))
(define-key global-map
  [menu-bar my fleet/add-url]
  '("copy URL to fleet note" . fleet/add-url))
(define-key global-map
  [menu-bar my fleet/add-region]
  '("copy region to fleet note" . fleet/add-region))
(define-key global-map
  [menu-bar my org-journal/add-region]
  '("copy region to journal" . org-journal/add-region))

;; make an apps menu, reflecting the apps on the global tool bar.
(define-key-after
  (lookup-key global-map [menu-bar])
  [apps]
  (cons "应用" (make-sparse-keymap "应用"))
  'my)
(define-key global-map
  [menu-bar apps glossary/flow]
  '("Dict" . glossary/flow))
(define-key global-map
  [menu-bar apps fleet/todo-visit]
  '("Todo" . fleet/todo-visit))
(define-key global-map
  [menu-bar apps gptel/start-or-send]
  '("GPT" . gptel/start-or-send))
(define-key global-map
  [menu-bar apps roam/visit-zettel]
  '("Roam" . roam/visit-zettel))
(define-key global-map
  [menu-bar apps org-journal/new-entry]
  '("Journal" . org-journal/new-entry))
(define-key global-map
  [menu-bar apps nov/visit-books]
  '("Books" . nov/visit-books))
(define-key global-map
  [menu-bar apps my/timer]
  '("Timer" . my/timer))
(define-key global-map
  [menu-bar apps @300/random-poem]
  '("Poems" . @300/random-poem))
(define-key global-map
  [menu-bar apps glossary/revisit]
  '("Glossary" . glossary/revisit))
(define-key global-map
  [menu-bar apps elfeed]
  '("Elfeed" . elfeed))
(define-key global-map
  [menu-bar apps my/visit-wikipedia]
  '("Wikipedia" . my/visit-wikipedia))
(define-key global-map
  [menu-bar apps eww-list-bookmarks]
  '("EWW Bookmark" . eww-list-bookmarks))
(define-key global-map
  [menu-bar apps eww]
  '("EWW" . eww))
(define-key global-map
  [menu-bar apps routine/visit-routine-file]
  '("Routine" . routine/visit-routine-file))
(define-key global-map
  [menu-bar apps diary]
  '("Diary" . diary))

;; volume buttons
(global-set-key (kbd "<volume-up>") 'scroll-down-command)
(global-set-key (kbd "<volume-down>") 'scroll-up-command)

;;; TAB for minibuffer and eshell
(define-key minibuffer-local-map (kbd "<volume-down>") 'minibuffer-complete)
(define-key minibuffer-local-completion-map (kbd "<volume-down>") 'minibuffer-complete)
(define-key minibuffer-local-completion-map (kbd "<volume-down>") 'minibuffer-complete)
(define-key minibuffer-local-filename-completion-map (kbd "<volume-down>") 'minibuffer-complete)
(define-key minibuffer-local-must-match-map (kbd "<volume-down>") 'minibuffer-complete)
(add-hook 'eshell-mode-hook
          #'(lambda ()
             (define-key eshell-mode-map (kbd "<volume-down>") 'completion-at-point)
             (define-key eshell-mode-map (kbd "<volume-up>") 'eshell-previous-input)))

;;; finalise startup apperance
(setq inhibit-startup-screen t
      initial-scratch-message nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; smaller font for header-line
            (set-face-attribute 'header-line nil :height 0.8)
            (switch-to-buffer "*Fancy Diary Entries*")
            (routine/check-and-warn)))

;;; org-roam
(require 'org-roam)
(setq org-roam-directory (concat emacs-dir "roam/")
      org-roam-db-location (concat emacs-dir "org-roam.db"))
;; hide property drawers
(require 'org-tidy)
(add-hook 'org-mode-hook #'org-tidy-mode)


(defun roam/visit-nodes ()
  (interactive)
  (find-file-noselect (concat emacs-dir "roam/"))
  (switch-to-buffer "roam")
  (dired-revert))

(defun roam/visit-zettel ()
  (interactive)
  ;; toggle between ethos and pathos.
  (pcase (buffer-name)
    ("pathos.org" (switch-to-buffer (find-file-noselect (concat emacs-dir "roam/ethos.org"))))
    (_ (switch-to-buffer (find-file-noselect (concat emacs-dir "roam/pathos.org"))))))

(defun roam/save ()
  (interactive)
  (save-buffer)
  ;; avoid running sync in normal org-mode
  (when (string= (file-name-directory (buffer-file-name)) (concat emacs-dir "roam/"))
    (org-roam-db-sync)))

(defvar org-tool-bar-map
  (let ((tool-bar-map (make-sparse-keymap)))
    (tool-bar-add-item "close" 'kill-current-buffer 'close)
    (tool-bar-add-item "open" 'roam/visit-nodes 'open)
    (tool-bar-add-item "undo" 'undo 'undo)
    (tool-bar-add-item "save" 'roam/save 'save)
    (tool-bar-add-item "copy" 'copy-region-as-kill 'copy)
    (tool-bar-add-item "search" 'isearch-forward 'search)
    (tool-bar-add-item "brain" 'roam/visit-zettel 'zettel)
    (tool-bar-add-item "right-arrow" 'org-roam-node-visit 'visit)
    (tool-bar-add-item "left-arrow" 'org-roam-buffer-toggle 'backlink)
    (tool-bar-add-item "plus" 'org-id-get-create 'add)
    (tool-bar-add-item "connect" 'org-roam-node-insert 'insert)
    (tool-bar-add-item "refresh" 'org-cycle-global 'cycle)
    tool-bar-map))

(add-hook 'org-mode-hook (lambda () (setq-local tool-bar-map org-tool-bar-map)))
(add-hook 'org-roam-mode-hook (lambda () (setq-local tool-bar-map org-tool-bar-map)))

;;; for PC
(unless (string= system-type "android")
  ;; evil
  (require 'evil)
  (evil-mode 1)
  ;; theme
  (require 'doom-themes)
  (load-theme 'doom-solarized-light t)
  ;; scrolling for nov.el and eww
  (with-eval-after-load 'nov
    (tool-bar-local-item "left-arrow" 'nov-scroll-down 'prev-screen nov-tool-bar-map)
    (tool-bar-local-item "right-arrow" 'nov-scroll-up 'next-screen nov-tool-bar-map))
  (with-eval-after-load 'eww
    (tool-bar-local-item "left-arrow" 'scroll-down-command 'prev-screen eww-tool-bar-map)
    (tool-bar-local-item "right-arrow" 'scroll-up-command 'next-screen eww-tool-bar-map))
  (with-eval-after-load 'elfeed
    (tool-bar-local-item "left-arrow" 'elfeed-show-prev 'prev-feed elfeed-tool-bar-map)
    (tool-bar-local-item "right-arrow" 'elfeed-show-next 'next-feed elfeed-tool-bar-map))
  )

