;;; properites-mode-tests.el --- ERT tests for properties-mode.  -*- lexical-binding: t -*-

;; Author: Iku Iwasa <iku.iwasa@gmail.com>
;; URL: https://github.com/iquiw/properties-mode

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

;;; Code:
(require 'cl-lib)
(require 'ert)

(when (require 'undercover nil t)
  (undercover "properties-mode.el"
              (:report-file "coverage-final.json")
              (:report-format 'codecov)
              (:send-report nil)))

(require 'properties-mode)

(transient-mark-mode 1)

(defmacro with-temp-properties-file (&rest body)
  "Create and open a temporary file and evaluate BODY."
  (declare (indent 0) (debug (body)))
  `(let ((file (make-temp-file "properties-test-" nil ".properties")))
     (unwind-protect
         (progn
           (let ((auto-mode-alist '()))
             (find-file file))
           (properties-mode)
           ,@body)
       (kill-buffer)
       (delete-file file))))

(defmacro with-properties-file (file &rest body)
  "Create and open a properties FILE and evaluate BODY."
  (declare (indent 1) (debug (form body)))
  `(progn
     (let ((auto-mode-alist '()))
       (find-file ,file))
     (unwind-protect
         (progn
           (properties-mode)
           ,@body)
       (kill-buffer))))

(ert-deftest properties-test-encode-buffer-with-ascii-only ()
  "Check ASCII only buffer is unchanged by `properties-encode-buffer'."
  (with-temp-buffer
    (insert "abc=123\ndef=456\nghijkl=foobar\n")
    (properties-encode-buffer)
    (should (equal (buffer-substring (point-min) (point-max))
                   "abc=123\ndef=456\nghijkl=foobar\n"))))

(ert-deftest properties-test-encode-buffer-with-multibytes ()
  "Check multibyte characters are encoded by `properties-encode-buffer'."
  (with-temp-buffer
    (insert "abc=123\ndef=456\nghijkl=あいう\n")
    (properties-encode-buffer)
    (should (equal (buffer-substring (point-min) (point-max))
                   "abc=123\ndef=456\nghijkl=\\u3042\\u3044\\u3046\n"))))

(ert-deftest properties-test-encode-buffer-to-uppercase ()
  "Check uppercase characters are used in escape if `properties-unicode-escape-uppercase' is t."
  (with-temp-buffer
    (insert "abc=123\ndef=456\nghijkl=漢字\n")
    (let ((properties-unicode-escape-uppercase t))
      (properties-encode-buffer))
    (should (equal (buffer-substring (point-min) (point-max))
                   "abc=123\ndef=456\nghijkl=\\u6F22\\u5B57\n"))))

(ert-deftest properties-test-decode-buffer-with-ascii-only ()
  "Check ASCII only buffer is unchanged by `properties-decode-buffer'."
  (with-temp-buffer
    (insert "abc=123\ndef=456\nghijkl=foobar\n")
    (properties-decode-buffer)
    (should (equal (buffer-substring (point-min) (point-max))
                   "abc=123\ndef=456\nghijkl=foobar\n"))))

(ert-deftest properties-test-decode-buffer-with-multibytes ()
  "Check unicode escaped characters are decoded by `properties-decode-buffer'."
  (with-temp-buffer
    (insert "abc=123\ndef=\\uff14\\uff15\\uFF16\nghijkl = foobar\n")
    (properties-decode-buffer)
    (should (equal (buffer-substring (point-min) (point-max))
                   "abc=123\ndef=４５６\nghijkl = foobar\n"))))

(ert-deftest properties-test-encode-region ()
  "Check multibyte characters in the region are encoded by `properties-encode-region'."
  (with-temp-buffer
    (insert "abc=あいう\ndef=いろは\nghijkl=○△□\n")
    (goto-char (point-min))
    (set-mark (point))
    (forward-line 2)
    (call-interactively #'properties-encode-region)
    (should (equal (buffer-substring (point-min) (point-max))
                   "abc=\\u3042\\u3044\\u3046\ndef=\\u3044\\u308d\\u306f\nghijkl=○△□\n"))))

(ert-deftest properties-test-decode-region ()
  "Check unicode escaped characters in the region are decoded by `properties-decode-region'."
  (with-temp-buffer
    (insert "abc=\\u3042\\u3044\\u3046\ndef=\\u3044\\u308d\\u306f\nghijkl=\\u25cb\\u25b3\\u25a1\n")
    (goto-char (point-min))
    (forward-line 1)
    (set-mark (point))
    (forward-line 2)
    (call-interactively #'properties-decode-region)
    (should (equal (buffer-substring (point-min) (point-max))
                   "abc=\\u3042\\u3044\\u3046\ndef=いろは\nghijkl=○△□\n"))))

(ert-deftest properties-test-load-encoded ()
  "Check unicode escaped characters are decoded at load."
  (with-temp-buffer
    (insert "abc=\\u3042\\u3044\\u3046\ndef=\\u3044\\u308d\\u306f\nghi=\\u25cb\\u25b3\\u25a1\n")
    (properties-mode)
    (should (equal (buffer-substring (point-min) (point-max))
                   "abc=あいう\ndef=いろは\nghi=○△□\n"))))

(ert-deftest properties-test-load-encoded-with-auto-escape-disabled ()
  "Check unicode escaped characters are decoded at load."
  (with-temp-buffer
    (insert "abc=\\u3042\\u3044\\u3046\ndef=\\u3044\\u308d\\u306f\nghi=\\u25cb\\u25b3\\u25a1\n")
    (setq properties-enable-auto-unicode-escape nil)
    (properties-mode)
    (setq properties-enable-auto-unicode-escape t)
    (should (equal (buffer-substring (point-min) (point-max))
                   "abc=\\u3042\\u3044\\u3046\ndef=\\u3044\\u308d\\u306f\nghi=\\u25cb\\u25b3\\u25a1\n"))))

(ert-deftest properties-test-save-encoded ()
  "Check multibyte characters are encoded at save."
  (with-temp-properties-file
    (insert "abc=あいう\ndef=いろは\nghi=○△□\n")
    (goto-char 8)
    (save-buffer)
    (should (equal (point) 8))
    (with-temp-buffer
      (insert-file-contents file)
      (should (equal (buffer-substring (point-min) (point-max))
                     "abc=\\u3042\\u3044\\u3046\ndef=\\u3044\\u308d\\u306f\nghi=\\u25cb\\u25b3\\u25a1\n")))))

(ert-deftest properties-test-save-encoded-with-auto-escape-disabled ()
  "Check multibyte characters are encoded at save."
  (with-temp-properties-file
    (insert "abc=あいう\ndef=いろは\nghi=○△□\n")
    (goto-char 8)
    (setq properties-enable-auto-unicode-escape nil)
    (save-buffer)
    (setq properties-enable-auto-unicode-escape t)
    (should (equal (point) 8))
    (with-temp-buffer
      (insert-file-contents file)
      (should (equal (buffer-substring (point-min) (point-max))
                     "abc=あいう\ndef=いろは\nghi=○△□\n")))))

(ert-deftest properties-test-save-preserve-eol-type ()
  "Check multibyte characters are encoded at save."
  (with-temp-properties-file
    (set-buffer-file-coding-system 'utf-8-dos)
    (insert "abc=123\n")
    (save-buffer)
    (with-temp-buffer
      (insert-file-contents file)
      (should (equal (coding-system-eol-type buffer-file-coding-system) 1)))))

(ert-deftest properties-test-save-not-modified-buffer ()
  "Check save is not processed if buffer is not modified."
  (with-properties-file "test/resources/message_ja.properties"
    (let (msg)
      (cl-letf (((symbol-function 'message) (lambda (&rest args)
                                              (setq msg (apply #'format args)))))
        (properties--save-buffer))
      (should (equal msg "(No changes need to be saved)")))))

(ert-deftest properties-test-change-mode-with-answer-y ()
  "Check buffer is encoded when changing to other mode if user answers \"y\"."
  (with-temp-buffer
    (let ((s "abc=\\u3042\\u3044\\u3046\ndef=\\u3044\\u308d\\u306f\nghi=\\u25cb\\u25b3\\u25a1\n"))
      (insert s)
      (properties-mode)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_p) t)))
        (conf-mode))
      (should (equal (buffer-substring (point-min) (point-max)) s)))))

(ert-deftest properties-test-change-mode-with-answer-n ()
  "Check buffer is not encoded when changing to other mode if user answers \"n\"."
  (with-temp-buffer
    (let ((s "abc=\\u3042\\u3044\\u3046\ndef=\\u3044\\u308d\\u306f\nghi=\\u25cb\\u25b3\\u25a1\n"))
      (insert s)
      (properties-mode)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_p) nil)))
        (conf-mode))
      (should (equal (buffer-substring (point-min) (point-max))
                     "abc=あいう\ndef=いろは\nghi=○△□\n")))))

(ert-deftest properties-test-change-mode-when-ascii-buffer-not-modified-with-answer-y ()
  "Check buffer is not modified if user answers \"y\" and all characters are ASCII when mode changed."
  (with-temp-properties-file
    (insert "abc=123\ndef=456\nghi=789n")
    (save-buffer)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_p) t)))
      (conf-mode))
    (should (not (buffer-modified-p)))))

(ert-deftest properties-test-change-mode-when-multibytes-buffer-not-modified-with-answer-y ()
  "Check buffer is not modified if user answers \"y\" and buffer contains multibyte characters."
  (with-temp-properties-file
    (insert "abc=あいう\ndef=いろは\nghi=○△□\n")
    (save-buffer)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_p) t)))
      (conf-mode))
    (should (not (buffer-modified-p)))))

(ert-deftest properties-test-change-mode-when-ascii-buffer-not-modified-with-answer-n ()
  "Check buffer is modified if user answer \"n\" and all characters are ASCII."
  (with-temp-properties-file
    (insert "abc=123\ndef=456\nghi=789n")
    (save-buffer)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_p) nil)))
      (conf-mode))
    (should (not (buffer-modified-p)))))

(ert-deftest properties-test-change-mode-when-multibytes-buffer-modified-with-answer-n ()
  "Check buffer is modified if user answers \"n\" and buffer contains multibyte characters."
  (with-temp-properties-file
    (insert "abc=あいう\ndef=いろは\nghi=○△□\n")
    (save-buffer)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_p) nil)))
      (conf-mode))
    (should (buffer-modified-p))
    (restore-buffer-modified-p nil)))

(ert-deftest properties-test-find-property-value-with-equal-separator ()
  "Check property value is found if separator is \"=\"."
  (with-temp-buffer
    (insert "abc=あいう\ndef = いろは\nghi = ○△□\n")
    (should (equal (properties--find-value "abc") "あいう"))
    (should (equal (properties--find-value "def") "いろは"))
    (should (not (properties--find-value "no-such-key")))))

(ert-deftest properties-test-find-property-value-with-colon-separator ()
  "Check property value is found if separator is \":\"."
  (with-temp-buffer
    (insert "abc: あいう\ndef: いろは\nghi: ○△□\n")
    (should (equal (properties--find-value "abc") "あいう"))
    (should (equal (properties--find-value "def") "いろは"))
    (should (not (properties--find-value "no-such-key")))))

(ert-deftest properties-test-get-property-key-success ()
  "Check property key at the current line is got."
  (with-temp-buffer
    (insert "abc=123\ndef-ghi: foo\njkl.mno = \n")
    (goto-char (point-min))
    (should (equal (properties--get-property-key) "abc"))
    (forward-line 1)
    (move-to-column 10)
    (should (equal (properties--get-property-key) "def-ghi"))
    (forward-line 1)
    (should (equal (properties--get-property-key) "jkl.mno"))))

(ert-deftest properties-test-get-property-key-nil ()
  "Check property key at the current line is nil if it is not available."
  (with-temp-buffer
    (insert "abc 123\n\n= foo\n")
    (goto-char (point-min))
    (should (not (properties--get-property-key)))
    (forward-line 1)
    (should (not (properties--get-property-key)))
    (forward-line 1)
    (should (not (properties--get-property-key)))))

(ert-deftest properties-test-get-reference-name-success ()
  "Check reference file name is got from the given name."
  (should (equal (properties--get-reference-name "message_ja.properties") "message_en.properties"))
  (should (equal (properties--get-reference-name "message_ja_JP.properties") "message_en.properties"))
  (should (equal (properties--get-reference-name "message_it.props") "message_en.props")))

(ert-deftest properties-test-get-reference-name-nil ()
  "Check reference file name is nil if the given name is not in proper format."
  (should (not (properties--get-reference-name "message.properties")))
  (should (not (properties--get-reference-name "message_japanese.properties")))
  (should (not (properties--get-reference-name "message-it.props"))))

(ert-deftest properties-test-get-reference-name-nil-for-reference-language ()
  "Check reference file name is nil if it is same as reference language"
  (should (not (properties--get-reference-name "message_en.properties"))))

(ert-deftest properties-test-find-reference-value-in-ja ()
  "Check reference value is got at the current line if current language is ja."
  (with-properties-file "test/resources/message_ja.properties"
    (goto-char (point-min))
    (should (equal (properties--find-reference-value) "en: Hello, world"))
    (forward-line 1)
    (should (equal (properties--find-reference-value) "en: Good morning!"))
    (forward-line 1)
    (should (equal (properties--find-reference-value) "en: Good evening!"))))

(ert-deftest properties-test-find-reference-value-in-en ()
  "Check reference value is nil if current language is same as reference."
  (with-properties-file "test/resources/message_en.properties"
    (goto-char (point-min))
    (should (not (properties--find-reference-value)))
    (forward-line 1)
    (should (not (properties--find-reference-value)))
    (forward-line 1)
    (should (not (properties--find-reference-value)))))

(ert-deftest properties-test-change-reference-language ()
  "Check reference value is got properly after reference language is changed."
  (with-properties-file "test/resources/message_ja.properties"
    (goto-char (point-min))
    (should (equal (properties--find-reference-value) "en: Hello, world"))
    (properties-change-reference-language nil "fr")
    (should (equal (properties--find-reference-value) "fr: Salut, le monde"))
    (properties-change-reference-language nil "en")))

(ert-deftest properties-test-change-reference-language-via-customize ()
  "Check reference value is got properly after customizing reference language."
  (with-properties-file "test/resources/message_ja.properties"
    (goto-char (point-min))
    (should (equal (properties--find-reference-value) "en: Hello, world"))
    (customize-set-variable 'properties-reference-language "fr")
    (forward-line 1)
    (should (equal (properties--find-reference-value) "fr: Bonjour!"))
    (customize-set-variable 'properties-reference-language "en")))

(ert-deftest properties-test-change-reference-language-globally ()
  "Check reference value is got properly after reference language is changed locally and then globally."
  (with-properties-file "test/resources/message_ja.properties"
    (goto-char (point-min))
    (should (equal (properties--find-reference-value) "en: Hello, world"))
    (properties-change-reference-language nil "fr")
    (should (equal (properties--find-reference-value) "fr: Salut, le monde"))
    (with-temp-buffer
      (properties-change-reference-language nil "en" t))
    (should (equal (properties--find-reference-value) "en: Hello, world"))))

(ert-deftest properties-test-revert-buffer-not-asking-encode ()
  "Check it does not ask encode buffer when revert buffer is performed."
  (with-properties-file "test/resources/message_ja.properties"
    (let (called)
      (cl-letf (((symbol-function 'properties--maybe-encode-buffer) (lambda () (setq called t))))
        (revert-buffer nil t))
      (should (not called)))))

(ert-deftest properties-test-revert-buffer-decode ()
  "Check it does not ask encode buffer when revert buffer is performed."
  (with-properties-file "test/resources/message_ja.properties"
    (revert-buffer nil t)
    (should (equal (buffer-substring (point-min) (point-max))
                   "message1=こんにちは世界\nmessage2=おはよう！\nmessage3=こんばんは！\n"))))

(ert-deftest properties-test-view-reference-file ()
  "Check it shows reference file by `properties-view-reference-file'."
  (with-properties-file "test/resources/message_ja.properties"
    (delete-other-windows)
    (let ((this-buf (current-buffer)))
      (forward-line 1)
      (properties-view-reference-file)
      (let ((ref-buf (get-file-buffer "message_en.properties")))
        (should ref-buf)
        (should (get-buffer-window ref-buf))
        (should (eq (current-buffer) this-buf))
        (with-current-buffer ref-buf
          (should (= (line-number-at-pos) 2)))))))

(ert-deftest properties-test-C-c-C-b-C-d-is-bound-to-decode-buffer ()
  "Check \"C-c C-b C-d\" is bound to `properties-decode-buffer'."
  (with-temp-buffer
    (switch-to-buffer (current-buffer))
    (properties-mode)
    (insert "abc=123\ndef=\\uff14\\uff15\\uFF16\nghijkl = foobar\n")
    (execute-kbd-macro (edmacro-parse-keys "C-c C-b C-d"))
    (should (equal (buffer-substring (point-min) (point-max))
                   "abc=123\ndef=４５６\nghijkl = foobar\n"))))

(ert-deftest properties-test-C-c-C-b-C-e-is-bound-to-encode-buffer ()
  "Check \"C-c C-b C-e\" is bound to `properties-encode-buffer'."
  (with-temp-buffer
    (switch-to-buffer (current-buffer))
    (properties-mode)
    (insert "abc=123\ndef=４５６\nghijkl = foobar\n")
    (execute-kbd-macro (edmacro-parse-keys "C-c C-b C-e"))
    (should (equal (buffer-substring (point-min) (point-max))
                   "abc=123\ndef=\\uff14\\uff15\\uff16\nghijkl = foobar\n"))))

(ert-deftest properties-test-C-c-C-d-is-bound-to-decode-region ()
  "Check \"C-c C-d\" is bound to `properties-decode-region'."
  (with-temp-buffer
    (switch-to-buffer (current-buffer))
    (properties-mode)
    (insert "abc=\\uff11\\uff12\\uff13\ndef=\\uff14\\uff15\\uFF16\nghijkl = foobar\n")
    (goto-char (point-min))
    (set-mark (point))
    (forward-line 1)
    (execute-kbd-macro (edmacro-parse-keys "C-c C-d"))
    (should (equal (buffer-substring (point-min) (point-max))
                   "abc=１２３\ndef=\\uff14\\uff15\\uFF16\nghijkl = foobar\n"))))

(ert-deftest properties-test-C-c-C-e-is-bound-to-encode-region ()
  "Check \"C-c C-e\" is bound to `properties-encode-region'."
  (with-temp-buffer
    (switch-to-buffer (current-buffer))
    (properties-mode)
    (insert "abc=１２３\ndef=４５６\nghijkl = foobar\n")
    (goto-char (point-min))
    (set-mark (point))
    (forward-line 1)
    (execute-kbd-macro (edmacro-parse-keys "C-c C-e"))
    (should (equal (buffer-substring (point-min) (point-max))
                   "abc=\\uff11\\uff12\\uff13\ndef=４５６\nghijkl = foobar\n"))))


(ert-deftest properties-test-C-c-C-l-is-bound-to-change-language ()
  "Check \"C-c C-l\" is bound to `properties-change-reference-language'."
  (with-properties-file "test/resources/message_ja.properties"
    (execute-kbd-macro (vconcat (edmacro-parse-keys "C-c C-l") "fr"))
    (should (equal properties-reference-language "fr"))))

(ert-deftest properties-test-C-c-C-v-is-bound-to-view-reference ()
  "Check \"C-c C-v\" is bound to `properties-view-reference-file'."
  (with-properties-file "test/resources/message_ja.properties"
    (delete-other-windows)
    (execute-kbd-macro (edmacro-parse-keys "C-c C-v"))
    (let ((ref-buf (get-file-buffer "message_en.properties")))
      (should ref-buf)
      (should (get-buffer-window ref-buf)))))

(provide 'properties-mode-tests)
;;; properties-mode-tests.el ends here
