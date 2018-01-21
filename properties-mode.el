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
           (insert (format "\\u%04x" c)))
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

(define-derived-mode properties-mode conf-mode "Props")

(provide 'properties-mode)
;;; properties-mode.el ends here
