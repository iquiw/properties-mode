;;; properties-mode.el --- Major mode to edit Java properties file  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Iku Iwasa

;; Author: Iku Iwasa <iku.iwasa@gmail.com>
;; Keywords: conf java

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a major mode to edit Java properties file.

;;; Code:

(defgroup properties nil
  "Major mode to edit Java properties file."
  :prefix "properties-"
  :group 'conf)

(defcustom properties-unicode-escape-uppercase nil
  "Whether to use uppercase characters to escape unicode.")

(defun properties-encode-buffer ()
  "Encode the current buffer to unicode escape characters."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (and (not (eobp))
                (re-search-forward "[^[:ascii:][:cntrl:]]+" nil t))
      (let ((s (match-string-no-properties 0)))
        (delete-region (match-beginning 0) (match-end 0))
        (mapc
         (lambda (c)
           (insert (format (if properties-unicode-escape-uppercase
                               "\\u%04X"
                             "\\u%04x")
                           c)))
         s)))))

(defun properties-decode-buffer ()
  "Decode unicode escape characters in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (and (not (eobp))
                (re-search-forward "\\\\u\\([0-9a-f]\\{4\\}\\)" nil t))
      (let ((s (match-string-no-properties 1)))
        (delete-region (match-beginning 0) (match-end 0))
        (insert (char-to-string (string-to-number s 16)))))))


(defun properties--save-buffer ()
  "Save properties mode buffer with encoded."
  (restore-buffer-modified-p
   (if (buffer-modified-p)
       (let ((name (buffer-name))
             (file (buffer-file-name))
             (start (point-min))
             (end (point-max))
             (origin (point))
             modified
             modified-time)
         (with-temp-buffer
           (insert-buffer-substring name start end)
           (properties-encode-buffer)
           (set-visited-file-name file t)
           (save-buffer)
           (setq modified (buffer-modified-p))
           (setq modified-time (visited-file-modtime)))
         (set-visited-file-modtime modified-time)
         modified)
     (message "(No changes need to be saved)")
     nil))
  t)

(define-derived-mode properties-mode conf-mode "Props"
  (when (eq major-mode 'properties-mode)
    (let ((modified (buffer-modified-p)))
      (properties-decode-buffer)
      (restore-buffer-modified-p modified))
;    (add-hook 'change-major-mode-hook 'properties--maybe-decode-buffer nil t)
    (add-hook 'write-contents-functions 'properties--save-buffer nil t)))

(provide 'properties-mode)
;;; properties-mode.el ends here
