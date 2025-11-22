;;; init-@300.el --- Custom configuration for @300. -*- lexical-binding: t; -*-

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

(provide 'init-@300)
;;; init-@300.el ends here
