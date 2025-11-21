;;; init-eww.el --- Custom configuration for EWW -*- lexical-binding: t; -*-

(require 'browse-url)
(require 'eww)
(require 'shr)

(with-eval-after-load 'eww
  (set-face-attribute 'shr-text nil :family "LXGW WenKai Screen"))

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

(defun eww/message-rss-feed ()
  "Scan the current EWW buffer's DOM for the first RSS/Atom link tag and message the URL."
  (let ((dom (plist-get eww-data :dom)))
    (when dom
      (dolist (link (dom-by-tag dom 'link))
        (let ((rel (dom-attr link 'rel))
              (type (dom-attr link 'type))
              (href (dom-attr link 'href)))
          ;; 1. Check if the link looks like an RSS/Atom feed
          (when (and (stringp rel) (string-match-p "alternate" rel)
                     (stringp type) (string-match-p "\\(rss\\|atom\\|xml\\)" type)
                     href)
            ;; 2. Echo the result and stop checking
            (message "RSS Feed Detected: %s" href)))))))
(add-hook 'eww-after-render-hook #'eww/message-rss-feed)

(provide 'init-eww)
;;; init-eww.el ends here
