(require 'cl-lib)
(require 'ert)

(when (require 'undercover nil t)
  (undercover "properties-mode.el"))

(require 'properties-mode)

(defmacro with-temp-properties-file (&rest body)
  "Create and open a temporary file and evaluate BODY."
  (declare (indent 0))
  `(let ((file (make-temp-file "properties-test-" nil ".properties")))
     (unwind-protect
         (progn
           (find-file file)
           (properties-mode)
           ,@body)
       (delete-file file))))

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

(ert-deftest properties-test-load-encoded ()
  "Check unicode escaped characters are decoded at load."
  (with-temp-buffer
    (insert "abc=\\u3042\\u3044\\u3046\ndef=\\u3044\\u308d\\u306f\nghi=\\u25cb\\u25b3\\u25a1\n")
    (properties-mode)
    (should (equal (buffer-substring (point-min) (point-max))
                   "abc=あいう\ndef=いろは\nghi=○△□\n"))))

(ert-deftest properties-test-save-encoded ()
  "Check multibyte characters are encoded at save."
  (with-temp-properties-file
    (insert "abc=あいう\ndef=いろは\nghi=○△□\n")
    (goto-char 8)
    (save-buffer)
    (should (equal (point) 8))
    (kill-buffer)
    (with-temp-buffer
      (insert-file-contents file)
      (should (equal (buffer-substring (point-min) (point-max))
                     "abc=\\u3042\\u3044\\u3046\ndef=\\u3044\\u308d\\u306f\nghi=\\u25cb\\u25b3\\u25a1\n")))))

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
    (should (buffer-modified-p))))

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

(ert-deftest properties-test-find-reference-value-in-ja ()
  "Check reference value is got at the current line if current language is ja."
  (find-file "test/resources/message_ja.properties")
  (unwind-protect
      (progn
        (properties-mode)
        (goto-char (point-min))
        (should (equal (properties--find-reference-value) "en: Hello, world"))
        (forward-line 1)
        (should (equal (properties--find-reference-value) "en: Good morning!"))
        (forward-line 1)
        (should (equal (properties--find-reference-value) "en: Good evening!")))
    (kill-buffer)
    (kill-buffer (get-file-buffer "test/resources/message_en.properties"))))

(ert-deftest properties-test-find-reference-value-in-en ()
  "Check reference value is nil if current language is same as reference."
  (find-file "test/resources/message_en.properties")
  (unwind-protect
      (progn
        (properties-mode)
        (goto-char (point-min))
        (should (not (properties--find-reference-value)))
        (forward-line 1)
        (should (not (properties--find-reference-value)))
        (forward-line 1)
        (should (not (properties--find-reference-value))))
    (kill-buffer)))

(ert-deftest properties-test-change-reference-language ()
  "Check reference value is got properly after reference language is changed."
  (find-file "test/resources/message_ja.properties")
  (unwind-protect
      (progn
        (properties-mode)
        (goto-char (point-min))
        (should (equal (properties--find-reference-value) "en: Hello, world"))
        (properties-change-reference-language "fr")
        (should (equal (properties--find-reference-value) "fr: Salut, le monde")))
    (properties-change-reference-language "en")
    (kill-buffer)
    (kill-buffer (get-file-buffer "test/resources/message_en.properties"))
    (kill-buffer (get-file-buffer "test/resources/message_fr.properties"))))
