;;; init-org-roam.el --- Custom configuration of org-roam. -*- lexical-binding: t; -*-

(require 'org)
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

(provide 'init-org-roam)
;;; init-org-roam.el ends here
