;;; stardict-es-en.el --- stardict extension for ES and EN -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Yu Huo
;;
;; Author: Yu Huo <yhuo@tuta.io>
;; Maintainer: Yu Huo <yhuo@tuta.io>
;; Created: August 18, 2025
;; Modified: August 18, 2025
;; Version: 0.0.1
;; Keywords: stardict
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 'stardict)
(require 'stem)
(require 'snowball-spanish)
(require 'persist)

(defun stardict--en-stemmer (word)
  (cdr (reverse (stem-english word))))

(defun stardict--es-stemmer (word)
  (let ((suffixes '("" "ar" "er" "ir" "o" "e" "a"))
        (stem (snowball-spanish-stem word)))
    (mapcar (lambda (suf) (concat stem suf)) suffixes)))

(defconst stardict-stemmer-list (list #'stardict--es-stemmer #'stardict--en-stemmer))
(defconst stardict-dir (file-name-directory (or load-file-name buffer-file-name)))
(defconst stardict-name-list '("SCCS" "langdao-ec-gb"))
(persist-defvar stardict-dict-hash-list nil "hash table list that stores the dictionaries.")

(defun stardict--remove-acute-accents (w)
  "Map áéíóúñ to aeioun in W."
  (replace-regexp-in-string
   "[áéíóúñ]" (lambda (m)
               (cl-case (aref m 0)
                 (?á "a") (?é "e") (?í "i") (?ó "o") (?ú "u") (?ñ "n")))
   w))

(define-derived-mode stardict-mode fundamental-mode "stardict")

(defun stardict--load-dict ()
  "load dictionaries when call for first time."
  (unless stardict-dict-hash-list
    (dolist (dict (reverse stardict-name-list))
      (push (stardict-open stardict-dir dict) stardict-dict-hash-list))))

(defun stardict--lookup-and-display (word dict-hash &optional HEAD)
  (with-current-buffer (get-buffer-create "*stardict*")
    (when HEAD (erase-buffer))
    (save-excursion
      (goto-char (point-max))
      (insert (concat word "\n"))
      (insert (stardict-lookup dict-hash word))
      (newline)
      (goto-char (point-min)))
    (stardict-mode)
    (switch-to-buffer (current-buffer))))

(defun stardict--lookup-and-return (word)
  (stardict--load-dict)
  (concat word "\n"
          (mapconcat #'identity
                     (cl-remove nil
                                (cl-loop for dict in stardict-dict-hash-list
                                         collect (stardict-lookup dict word)))
                     "\n")))

(defun stardict--check-and-stem (word)
  (let ((word-list nil)
        (d-list stardict-dict-hash-list)
        (s-list stardict-stemmer-list)
        (head t))
    (cl-loop for i from 0 below (min (length d-list) (length s-list))
             do (if (stardict-word-exist-p (nth i d-list) (stardict--remove-acute-accents word))
                    (push (stardict--remove-acute-accents word) word-list)
                  ;; if word is not in dictionary, try to stem (with the original accented form)
                  (let* ((word-candidates (funcall (nth i s-list) word))
                         ;; remove ñ after stemming
                         (word-candidates-de-accented (mapcar #'stardict--remove-acute-accents word-candidates)))
                    (catch 'found
                      (dolist (wd word-candidates-de-accented)
                        (when (stardict-word-exist-p (nth i d-list) wd)
                          (push wd word-list)
                          (throw 'found wd)))
                      ;; if nothing is caught
                      (push nil word-list)))))
    (if (cl-every 'null word-list)
        (message "No definition is found!")
      (cl-loop for i from 0 below (length word-list)
               do (let ((wd (nth i (reverse word-list))))
                    (when wd
                      (stardict--lookup-and-display wd (nth i d-list) head)
                      (setq head nil)))))))

(defun stardict-define-at-point ()
  "Define the word at point."
  (interactive)
  (stardict--load-dict)
  (let ((word (downcase (thing-at-point 'word))))
    (stardict--check-and-stem word)))

(defun stardict-define (word)
  "Prompt for `WORD' and define it."
  (interactive "sWord: ")
  (stardict--load-dict)
  (setq word (string-trim (stardict--remove-acute-accents (downcase word))))
  (let ((flag nil)
        (head t))
    (cl-loop for i from 0 below (length stardict-dict-hash-list)
             do (let ((dict-hash (nth i stardict-dict-hash-list)))
                  (when (stardict-word-exist-p dict-hash word)
                    (stardict--lookup-and-display word dict-hash head)
                    (setq flag 'success)
                    (setq head nil))))
    (unless flag
      (message "No definition is found!"))))

(provide 'stardict-es-en)
;;; stardict-duo.el ends here
