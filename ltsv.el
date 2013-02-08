;;; ltsv.el --- LTSV for Emacs

;; Copyright (C) 2013 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-ltsv
;; Version: 0.01

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl))

(defun ltsv:mklist (obj)
  (if (listp obj) obj (list obj)))

(defun ltsv:key-valid-p (key want ignore)
  (let ((wants (ltsv:mklist want))
        (ignores (ltsv:mklist ignore)))
    (and (if ignore
             (not (member key ignores))
           t)
         (if want
             (member key wants)
           t))))

(defun* ltsv:parse-line (line &key want ignore)
  (loop with chomped = (replace-regexp-in-string "\n\\'" "" line)
        for pair in (split-string chomped "\t")
        do (string-match "\\`\\([^:]+\\):\\(.+\\)\\'" pair)
        for key = (match-string-no-properties 1 pair)
        for val = (match-string-no-properties 2 pair)
        when (ltsv:key-valid-p key want ignore)
        collect (cons key val)))

(defun* ltsv:parse-file (file &key want ignore)
  (save-excursion
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-min))
      (loop while (not (eobp))
            for line = (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position))
            collect
            (prog1
                (ltsv:parse-line line :want want :ignore ignore)
              (forward-line 1))))))

(defun ltsv:to-string (kv-lst)
  (let ((joined-kv (loop for (key . value) in kv-lst
                         collect (format "%s:%s" key value))))
    (mapconcat 'identity joined-kv "\t")))

(defvar ltsv:view-buffer "*ltsv-view*")

(defface ltsv-view-key
  '((t (:foreground "magenta")))
  "Face of LTSV key"
  :group 'ltsv)

(defface ltsv-view-value
  '((t (:foreground "green")))
  "Face of LTSV value"
  :group 'ltsv)

(define-generic-mode ltsv-view-mode
  nil
  nil
  '(("^ \\([^:]+\\): \\(.+?\\)$"
     (1 'ltsv-view-key)
     (2 'ltsv-view-value)))
  nil nil
  "Major mode for LTSV view mode")

(defun* ltsv:view-buffer (&key want ignore)
  (interactive)
  (ltsv:view-file (buffer-file-name) :want want :ignore ignore))

(defun* ltsv:view-file (file &key want ignore)
  (interactive
   (list (read-file-name "Input LTSV file: " nil (buffer-file-name))))
  (let ((kvs-lst (ltsv:parse-file file :want want :ignore ignore)))
    (with-current-buffer (get-buffer-create ltsv:view-buffer)
      (setq buffer-read-only nil)
      (erase-buffer)
      (goto-char (point-min))
      (insert "----\n")
      (loop for kv-lst in kvs-lst
            do
            (progn
              (loop for (key . value) in kv-lst
                    do
                    (insert (format " %s: %s\n" key value)))
              (insert "----\n")))
      (pop-to-buffer (current-buffer))
      (setq buffer-read-only t)
      (ltsv-view-mode)
      (goto-char (point-min)))))

(provide 'ltsv)

;;; ltsv.el ends here
