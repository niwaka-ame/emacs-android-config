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

;;; calendar, diary, appointment and routines
(require 'init-diary)

;;; shr
(require 'shr)
;; preventing shr from adding hard line break when rendering.
;; see https://emacs.stackexchange.com/questions/31882/how-to-prevent-eww-from-truncating-lines
;; requires for nov-grep to function normally
(eval-after-load 'shr
  '(progn (setq shr-width -1)
          (defun shr-fill-text (text) text)
          (defun shr-fill-lines (start end) nil)
          (defun shr-fill-line () nil)))

;;; EWW
(require 'init-eww)

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
;; hide property drawers
(require 'org-tidy)
(add-hook 'org-mode-hook #'org-tidy-mode)

;;; fleeting notes
(require 'fleet)

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

;;; poems
(require 'init-@300)

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
(require 'init-elfeed)

;;; org-journal
(require 'init-org-journal)

;;; gptel
(require 'init-gptel)

;;; org-roam
(require 'org-roam)
(setq org-roam-directory (concat emacs-dir "roam/")
      org-roam-db-location (concat emacs-dir "org-roam.db"))

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

