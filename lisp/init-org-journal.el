;;; init-org-journal.el --- Custom configuration for org-journal. -*- lexical-binding: t; -*-

(require 'org-journal)
(require 'grep)

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

(provide 'init-org-journal)
;;; init-org-journal.el ends here
