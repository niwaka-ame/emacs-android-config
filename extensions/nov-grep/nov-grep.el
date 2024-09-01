;; from https://gist.github.com/snippins/c8c9ade536881d1def67c2b69e51ed82
(require 'counsel)
(defun my-nov-find-id-of-file (filename)
  (interactive)
  (let ((i 0)
        (dontbreak t))
    (while (and (< i (length nov-documents))
                dontbreak)
      (setq i (1+ i))
      (when (string-equal filename (cdr (aref nov-documents i)))
        (setq dontbreak nil)
        (setq nov-documents-index i)))))
(defun my-nov-counsel-rg (&optional initial-input initial-directory extra-rg-args rg-prompt)
  (interactive)
  ;; require rg with pcre2 support: cargo install ripgrep --features 'pcre2'
  (setq-local counsel-rg-base-command "rg --pcre2 -U --multiline-dotall -L --column --hidden -S --no-heading --line-number --color never %s")
  (setq-local ivy-re-builders-alist
              '((t      . ivy--regex-plus)
                (my-nov-counsel-ag      . my-nov-ivy--regex-plus)))
  (let ((counsel-ag-base-command
         (concat counsel-rg-base-command (counsel--rg-targets)))
        (counsel--grep-tool-look-around
         (let ((rg (car (split-string counsel-rg-base-command)))
               (switch "--pcre2"))
           (and (eq 0 (call-process rg nil nil nil switch "--version"))
                switch))))
    (my-nov-counsel-ag initial-input initial-directory extra-rg-args rg-prompt
                       :caller 'counsel-rg)))
(defun my-nov-counsel-git-grep-action (x)
  "Go to occurrence X in current Git repository."
  (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\([0-9]+\\):\\(.*\\)\\'" x)
    (let ((file-name (match-string-no-properties 1 x))
          (line-number (string-to-number (match-string-no-properties 2 x)))
          (column-number (string-to-number (match-string-no-properties 3 x)))
          (paragraph-number 0)
          (table-number 0)
          (table-element-number 0)
          (posend 0)
          (search-string "")
          (recenter-positions '(top))
          (list-words)
          (case-fold-search t))
      (with-temp-buffer
        (insert-file-contents file-name)
        (forward-line (- line-number 1))
        (move-to-column column-number)
        (setq posend (point))
        (goto-char 0)
        ;; get number of <p> tags
        (while (and (< (point) posend)
                    (search-forward "<p" posend t))
          (when (and (not (string-match "> <" (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
                     (not (string-match ">&nbsp;<" (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
                     (not (string-match ">&#160;<" (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
                     (not (string-match "<p.*<img" (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
                     (not (string-match "<p.[^<]*>$" (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
                     (string-match ">[^<]+</" (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
            (setq paragraph-number (+ paragraph-number 1))))
        ;; get number of <table> tags
        (goto-char 0)
        (while (and (< (point) posend)
                    (search-forward "<table" posend t))
          (setq table-number (+ table-number 1)))
        ;; get number of <td> tag
        (goto-char 0)
        (while (and (< (point) posend)
                    (search-forward "<td" posend t))
          (setq table-element-number (+ table-element-number 1))))
      ;; jump to the chapter with the match
      (my-nov-find-id-of-file
       (expand-file-name file-name
                         (ivy-state-directory ivy-last)))
      (call-interactively 'nov-render-document)
      ;; add to swiper history
      (add-to-list 'swiper-history (car counsel-git-grep-history))
      ;;(message "ps: %s" paragraph-number)
      ;; jump to the paragraphs contain the match
      (goto-char 0)
      (forward-paragraph (- (- paragraph-number 1) (- table-element-number table-number)))
      ;; goto and highlight the match
      (recenter-top-bottom)
      (setq search-string (car counsel-git-grep-history))
      (setq case-fold-search (ivy--case-fold-p search-string))
      (setq list-words (delete "" (split-string search-string)))
      (search-forward-regexp
       (replace-regexp-in-string "\"" "[\"“”]" (replace-regexp-in-string "'" "['’]" (car list-words))))
      (push-mark (match-beginning 0) t t)
      (setq list-words (cdr list-words))
      (dolist (elt list-words)
        (setq elt (replace-regexp-in-string "'" "['’]" elt))
        (setq elt (replace-regexp-in-string "\"" "[\"“”]" elt))
        (search-forward-regexp elt))
      (setq deactivate-mark nil)
      (activate-mark)
      (recenter))))
(defun my-nov-ivy--regex-plus (str)
  ;; (message "str: %s" ivy-text)
  (setq my-nov-trace nil)
  (setq my-nov-trace (nth 1 (backtrace-frame 4)))
  (setq str (replace-regexp-in-string " $" "" str))
  (setq str (replace-regexp-in-string "'" "['’]" str))
  (setq str (replace-regexp-in-string "\"" "[\"“”]" str))
  (if my-nov-paragraph-multiline-global
      (setq str (replace-regexp-in-string " +" "\\\\(\\\\(?!<p\\\\).\\\\)*[^0-9[:alpha:]]" str))
    (setq str (replace-regexp-in-string " +" "[^\n]*?[^0-9[:alpha:]]" str)))
  str)
(cl-defun my-nov-counsel-ag (&optional initial-input initial-directory extra-ag-args ag-prompt
                                       &key caller)
  (interactive)
  (setq counsel-ag-command counsel-ag-base-command)
  (setq counsel--regex-look-around counsel--grep-tool-look-around)
  (counsel-require-program counsel-ag-command)
  (when current-prefix-arg
    (setq initial-directory
          (or initial-directory
              (counsel-read-directory-name (concat
                                            (car (split-string counsel-ag-command))
                                            " in directory: "))))
    (setq extra-ag-args
          (or extra-ag-args
              (read-from-minibuffer (format
                                     "%s args: "
                                     (car (split-string counsel-ag-command)))))))
  (setq counsel-ag-command (counsel--format-ag-command (or extra-ag-args "") "%s"))
  (let ((default-directory (or initial-directory
                               (counsel--git-root)
                               default-directory)))
    (ivy-read (or ag-prompt
                  (concat (car (split-string counsel-ag-command)) ": "))
              #'counsel-ag-function
              :initial-input initial-input
              :dynamic-collection t
              :keymap counsel-ag-map
              :history 'counsel-git-grep-history
              :action #'my-nov-counsel-git-grep-action
              :require-match t
              :re-builder 'my-nov-ivy--regex-plus
              :caller (or caller 'my-nov-counsel-ag))))
(defun my-nov-grep (&optional input)
  (interactive)
  (setq my-nov-paragraph-multiline-global nil)
  (unwind-protect
      (if (file-exists-p nov-temp-dir)
          (my-nov-counsel-rg input nov-temp-dir " -g '*.*htm*'" "nov-grep: ")
        ;; (counsel-rg input nov-temp-dir " -g '*.*htm*'" "nov-grep: ")
        (user-error (format "%S does not exist" nov-temp-dir)))
    (when (not (string-equal ivy-text (car counsel-git-grep-history)))
      (cl-pushnew ivy-text counsel-git-grep-history))))
(defun my-nov-grep-multiline (&optional input)
  (interactive)
  (setq my-nov-paragraph-multiline-global t)
  (unwind-protect
      (if (file-exists-p nov-temp-dir)
          (my-nov-counsel-rg input nov-temp-dir " -g '*.*htm*'" "nov-grep: ")
        (user-error (format "%S does not exist" nov-temp-dir)))
    (when (not (string-equal ivy-text (car counsel-git-grep-history)))
      (cl-pushnew ivy-text counsel-git-grep-history))))


(provide 'nov-grep)
;;; nov-grep.el ends here
