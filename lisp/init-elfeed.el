;;; init-elfeed.el --- Custom configuration for elfeed -*- lexical-binding: t; -*-

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
    (dolist (pair elfeed/filter-alist)
        (tool-bar-add-item (symbol-name (car pair))
                           (eval `(lambda () (interactive) (elfeed/set-preset-filter ,(cdr pair))))
                           (car pair)))
    (tool-bar-add-item "dict" 'glossary/define-at-point 'dict)
    (tool-bar-add-item "robot" 'gptel/ask-llama 'GPT)
    tool-bar-map))
(add-hook 'elfeed-search-mode-hook (lambda () (setq-local tool-bar-map elfeed-tool-bar-map)))
(add-hook 'elfeed-search-mode-hook (lambda () (text-scale-set -3)))
(add-hook 'elfeed-search-mode-hook (lambda () (setq-local line-spacing 0.2)))
(add-hook 'elfeed-show-mode-hook (lambda () (setq-local tool-bar-map elfeed-tool-bar-map)))
(add-hook 'elfeed-show-mode-hook #'visual-line-mode)
(add-hook 'elfeed-show-mode-hook (lambda () (text-scale-set -1)))

(provide 'init-elfeed)
;;; init-elfeed.el ends here
