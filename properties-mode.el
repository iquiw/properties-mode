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
  "Encode unicode escape characters in the current buffer."
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

(defvar properties--in-save-buffer nil)

(defun properties--save-buffer ()
  "Save properties mode buffer with encoded."
  ;; derived from `hexl-save-buffer'.
  (unless properties--in-save-buffer
    (restore-buffer-modified-p
     (if (buffer-modified-p)
         (let ((name (buffer-name))
               (start (point-min))
               (end (point-max))
               modified)
           (with-temp-buffer
             (let ((buf (current-buffer)))
               (insert-buffer-substring name start end)
               (set-buffer name)
               (properties-encode-buffer)
               ;; Prevent infinite recursion.
               (let ((properties--in-save-buffer t))
                 (save-buffer))
               (setq modified (buffer-modified-p))
               (delete-region (point-min) (point-max))
               (insert-buffer-substring buf start end)
               modified)))
       (message "(No changes need to be saved)")
       nil))
    ;; Return t to indicate we have saved
    t))

(define-derived-mode properties-mode conf-mode "Props"
  (when (eq major-mode 'properties-mode)
    (let ((modified (buffer-modified-p)))
      (properties-decode-buffer)
      (restore-buffer-modified-p modified))
    (add-hook 'write-contents-functions 'properties--save-buffer nil t)))

(provide 'properties-mode)
;;; properties-mode.el ends here
