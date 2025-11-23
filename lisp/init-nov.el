;;; init-nov.el --- Custom configuration for nov.el  -*- lexical-binding: t; -*-

(require 'nov)
(require 'shr)

(with-eval-after-load 'nov
  ;; 1. DEFINITION: The Toggle Variable
  (defvar nov-enable-link-peeking t
    "When non-nil, preview internal links in the minibuffer instead of jumping.
This is automatically disabled when viewing the Table of Contents.")

  ;; 2. COMMAND: Toggle Function
  (defun nov-toggle-link-peeking ()
    "Toggle the peeking behavior for internal links."
    (interactive)
    (setq nov-enable-link-peeking (not nov-enable-link-peeking))
    (message "Link peeking %s" (if nov-enable-link-peeking "enabled" "disabled")))

  ;; 3. HELPER: The Smart Content Fetcher
  (defun nov-content-at-target (filename target)
    "Retrieve text content from FILENAME at anchor TARGET.
Smartly handles duplicates by ignoring self-referencing links (source anchors)
while accepting back-links (target anchors)."
    (let* ((current-path (cdr (aref nov-documents nov-documents-index)))
           (directory (file-name-directory current-path))
           (full-path (if (or (null filename) (string= filename ""))
                          current-path
                        (if (file-name-absolute-p filename)
                            filename
                          (file-truename (nov-make-path directory filename))))))
      (when (file-exists-p full-path)
        (with-temp-buffer
          (insert-file-contents full-path)
          (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
                 ;; A "real" target has the ID but is NOT a link to that same ID
                 (is-real-target-p
                  (lambda (node)
                    (and (equal (dom-attr node 'id) target)
                         (let ((href (dom-attr node 'href)))
                           (or (null href)
                               (not (string-suffix-p (concat "#" target) href)))))))
                 (node (seq-find is-real-target-p (dom-search dom is-real-target-p))))

            (when node
              ;; If the node is inline, find its block-level parent
              (when (memq (dom-tag node) '(a span sup sub em strong i b font small))
                (setq node
                      (or (seq-find (lambda (block)
                                      (dom-search block is-real-target-p))
                                    (append (dom-by-tag dom 'li)
                                            (dom-by-tag dom 'aside)
                                            (dom-by-tag dom 'p)
                                            (dom-by-tag dom 'div)
                                            (dom-by-tag dom 'dd)))
                          node)))
              (replace-regexp-in-string "[ \t\n]+" " " (dom-texts node " "))))))))

  ;; 4. OVERRIDE: The Logic to Check TOC and Toggle State
  (defun nov-browse-url (&optional mouse-event)
    "Follow external links, or preview internal footnotes if enabled and valid."
    (interactive (list last-nonmenu-event))
    (mouse-set-point mouse-event)
    (let ((url (get-text-property (point) 'shr-url)))
      (when (not url)
        (user-error "No link under point"))

      ;; LOGIC CHECK:
      ;; 1. Is peeking enabled?
      ;; 2. Are we currently OUTSIDE the Table of Contents?
      ;; 3. Is it an internal link?
      (let* ((curr-doc (aref nov-documents nov-documents-index))
             (curr-id (car curr-doc))
             (is-toc (eq curr-id nov-toc-id)) ;; nov.el specific check
             (should-peek (and nov-enable-link-peeking
                               (not is-toc)
                               (not (nov-external-url-p url)))))

        (if (not should-peek)
            ;; STANDARD BEHAVIOR (Jump)
            (if (nov-external-url-p url)
                (browse-url url)
              (apply 'nov-visit-relative-file (nov-url-filename-and-target url)))

          ;; PEEK BEHAVIOR
          (seq-let (filename target) (nov-url-filename-and-target url)
            (let ((preview-text (and target (nov-content-at-target filename target))))
              (if preview-text
                  (message "%s" preview-text)
                ;; Fallback to jump if peek fails
                (apply 'nov-visit-relative-file (list filename target))))))))))

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

;;; literature notes
(require 'org)
(require 'seq)

(defvar hlt/quote-directory (concat emacs-dir "hl-notes/"))

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
            (find-file-noselect (concat hlt/quote-directory buf-name ".org"))
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
    (concat hlt/quote-directory (substring (buffer-name) 0 (- (length (buffer-name)) 5)) ".org")))
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

;; drawing random quote to display
;; 1. Define a variable to hold the cache. Initial value is nil.
(defvar hlt/quote-cache nil
  "A cached list of (Quote . Book) pairs. Populated lazily.")

(defun hlt/collect-all-quotes ()
  "Scan all org files and return a list of (Quote . Book) pairs."
  (let ((files (directory-files hlt/quote-directory t "\\.org$")))
    (apply #'append
           (mapcar
            (lambda (file)
              (let ((book-title (file-name-base file)))
                (with-temp-buffer
                  (insert-file-contents file)
                  (delay-mode-hooks (org-mode))
                  (org-map-entries
                   (lambda ()
                     (cons (org-get-heading t t t t) book-title))))))
            files))))

(defun hlt/random-quote (&optional force-refresh)
  "Display a random quote.
   If the cache is empty, it loads the quotes from disk.
   Run with prefix argument (C-u) to force a reload from disk."
  (interactive "P") ;; "P" allows the function to read the C-u prefix

  ;; 2. Logic: If cache is nil OR user requested a refresh, load from disk
  (when (or force-refresh (null hlt/quote-cache))
    (message "Loading quotes from disk...")
    (setq hlt/quote-cache (hlt/collect-all-quotes))
    (message "Loaded %d quotes." (length hlt/quote-cache)))

  ;; 3. Draw from the cache
  (if (not hlt/quote-cache)
      (message "No quotes found in %s" hlt/quote-directory)
    (let* ((selected-pair (seq-random-elt hlt/quote-cache))
           (quote-text (car selected-pair))
           (book-title (cdr selected-pair)))
      (message "[%s]: %s" book-title quote-text))))

(provide 'init-nov)
;;; init-nov.el ends here
