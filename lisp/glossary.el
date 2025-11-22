;;; glossary.el --- Defining functionalities of glossary. -*- lexical-binding: t; -*-

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

(provide 'glossary)
;;; glossary.el ends here
