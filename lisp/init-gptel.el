;;; init-gptel.el --- Custom configuration for gptel. -*- lexical-binding: t; -*-

(require 'markdown-mode)
(require 'gptel)

;; setq `gptel-directives'
(load (concat emacs-dir "llama-directives.el"))
;; make thing-at-point recognise sentence with single space.
(setq sentence-end-double-space nil)

;; OpenRouter offers an OpenAI compatible API
(setq gptel-model 'tngtech/deepseek-r1t2-chimera:free
      gptel-max-tokens 2000
      gptel-include-reasoning nil
      gptel-backend
      (gptel-make-openai "OpenRouter"               ;Any name you want
                         :host "openrouter.ai"
                         :endpoint "/api/v1/chat/completions"
                         :stream t
                         :key (with-current-buffer (find-file-noselect (concat emacs-dir "llama")) (buffer-substring-no-properties (point-min) (1- (point-max))))
                         :models '(tngtech/deepseek-r1t2-chimera:free)))
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

(provide 'init-gptel)
;;; init-gptel.el ends here
