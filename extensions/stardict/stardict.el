;;; stardict.el --- stardict dictionary library

;; Copyright (C) 2010 Changyuan Yu

;; Author: Changyuan Yu <rei.vzy@gmail.com>
;; Created: 2010-11-06
;; Version: 0.1
;; Keywords: stardict

;; This file is *NOT* part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to andyetitmoves@gmail.com)
;; or from the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:
;;

;; Example:
;;
;; (require 'stardict)
;; (setq dict
;;       (stardict-open "~/.stardict/dic/stardict-lazyworm-ec-2.4.2"
;;                      "lazyworm-ec"))
;; (stardict-word-exist-p dict "apple")
;; (stardict-lookup dict "apple")

;; (stardict-open-dict-file dict)
;; (mapcar (lambda (x) (stardict-lookup dict x)) (make-list 1000 "apple"))


;;; Code:
(require 'stem)
(require 'persist)
(defconst stardict-dir (file-name-directory (or load-file-name buffer-file-name)))
(defconst stardict-name "langdao-ec-gb")
(persist-defvar stardict-dict-hash nil "hash table that stores the dictionary.")

(defun stardict-str2int (str)
  "Convert string `STR' to integer.
\x21\x22 => 0x2122"
  (let ((sum 0))
    (mapc (lambda (c)
            (setq sum (+ (* sum #x100)
                         (mod c #x100))))
          str)
    sum))

(defun stardict-open (dir name &optional nocache)
  "Open stardict dictionary in directory `DIR' with name `NAME'.
When `NOCACHE' is not nil, don't load from cache and save to cache.
The return is used as `DICT' argument in other functions."
  (if nocache (stardict-open-1 dir name)
    (let ((cache (expand-file-name (concat name ".idx.emacs.bz2") dir)) ret)
      (if (file-exists-p cache)
          (with-temp-buffer
            (insert-file-contents cache)
            (read (current-buffer)))
        (setq ret (stardict-open-1 dir name))
        (with-temp-buffer
          (prin1 ret (current-buffer))
          (write-region nil nil cache))
        ret))))

(defun stardict-open-1 (dir name)
  "Internal function used by `stardict-open'.
`DIR' is dictionary location, `NAME' is dictionary name."
  (let ((ifo  (expand-file-name (concat name ".ifo") dir))
        (idx  (expand-file-name (concat name ".idx") dir))
        (dict (expand-file-name (concat name ".dict") dir))
        (idx-offset-bytes 4)
        (word-count 0)
        ifo-ht idx-ht)
    (unless (file-exists-p idx)
      (setq idx (concat idx ".gz")))
    (unless (file-exists-p dict)
      (setq dict (concat dict ".dz")))
    ;;(message "List %S" (list idx dict ifo))
    (unless (and (file-exists-p idx)
                 (file-exists-p dict)
                 (file-exists-p ifo))
      (error "File not found"))
    (setq ifo-ht (make-hash-table :test 'equal))
    (setq idx-ht (make-hash-table :test 'equal))
    ;; get info
    (with-temp-buffer
      (insert-file-contents ifo)
      (goto-char (point-min))
      (while (re-search-forward "^\\([a-zA-Z]+\\)=\\(.*\\)$" nil t)
        (puthash (match-string 1) (match-string 2) ifo-ht)))
    (when (gethash "idxoffsetbits" ifo-ht)
      (setq idx-offset-bytes
            (/ (string-to-number (gethash "idxoffsetbits" ifo-ht)) 8)))
    (setq word-count
          (string-to-number (gethash "wordcount" ifo-ht)))
    ;; get index
    (with-temp-buffer
      (insert-file-contents idx)
      (goto-char (point-min))
      (let ((rpt (make-progress-reporter "read index: " 0 (1- word-count))))
        (dotimes (i word-count)
          (progress-reporter-update rpt i)
          (let (p word offset size)
            (re-search-forward "\\([^\x00]+?\\)\x00" nil t)
            (setq p (point))

            (setq word (match-string 1))
            (setq offset
                  (stardict-str2int
                   (buffer-substring-no-properties p
                                                   (+ p idx-offset-bytes))))
            (setq size
                  (stardict-str2int
                   (buffer-substring-no-properties (+ p idx-offset-bytes)
                                                   (+ p idx-offset-bytes 4))))
            (forward-char (+ idx-offset-bytes 4))
            (puthash word (cons offset size) idx-ht)
            )))
      (list ifo-ht idx-ht dict))))

(defun stardict-word-exist-p (dict word)
  "Checkout whether `WORD' existed in `DICT'."
  (gethash word (nth 1 dict)))

(defun stardict-lookup (dict word)
  "Lookup `WORD' in `DICT', return nil when not found."
  (let ((info (gethash word (nth 1 dict)))
        (file (nth 2 dict))
        buffer
        offset size begin end)
    (when info
      (setq offset (car info))
      (setq size (cdr info))
      ;; find any opened dict file
      (dolist (buf (buffer-list))
        (when (equal file (buffer-file-name buf))
          (setq buffer buf)))
      (if buffer
          (with-current-buffer buffer
            (buffer-substring-no-properties (byte-to-position (1+ offset))
                                            (byte-to-position (+ 1 offset size))))
        (with-temp-buffer
          (insert-file-contents (nth 2 dict) nil offset (+ offset size))
          (buffer-string))))))

(defun stardict-open-dict-file (dict)
  "Open dict file of `DICT' in Emacs to speed up word lookup.
You should close the dict file yourself."
  (with-current-buffer (find-file-noselect (nth 2 dict))
    (setq buffer-read-only t)))

(define-derived-mode stardict-mode fundamental-mode "stardict")

(defun stardict--load-dict ()
  "load dictionary when call for first time."
  (unless stardict-dict-hash
    (setq stardict-dict-hash
          (stardict-open stardict-dir stardict-name))))

(defun stardict--lookup-and-display (word)
  (with-current-buffer (get-buffer-create "*stardict*")
    (erase-buffer)
    (insert (concat word "\n"))
    (insert (stardict-lookup stardict-dict-hash word))
    (goto-char (point-min))
    (stardict-mode)
    (switch-to-buffer (current-buffer))))

(defun stardict--lookup-and-return (word)
  (stardict--load-dict)
  (concat word "\n" (stardict-lookup stardict-dict-hash word)))

(defun stardict-define-at-point ()
  "Define the word at point."
  (interactive)
  (stardict--load-dict)
  (let ((word (downcase (thing-at-point 'word))))
    ;; if word is not in dictionary, try to truncate
    (unless (stardict-word-exist-p stardict-dict-hash word)
      (let ((word-list (cdr (reverse (stem-english word)))))
        (catch 'found
          (dolist (wd word-list)
            (when (stardict-word-exist-p stardict-dict-hash wd)
              (setq word wd)
              (throw 'found wd)))
          ;; if nothing is caught
          (setq word nil))))
    (if word
        (stardict--lookup-and-display word)
      (message "No definition is found!"))))

(defun stardict-define (word)
  "Prompt for `WORD' and define it."
  (interactive "sWord: ")
  (stardict--load-dict)
  (setq word (string-trim (downcase word)))
  (if (stardict-word-exist-p stardict-dict-hash word)
      (stardict--lookup-and-display word)
    (message "No definition is found!")))

(provide 'stardict)

;;; stardict.el ends here
