(require 'cl-lib)
(require 'ert)
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

(ert-deftest properties-encode-buffer-with-ascii-only ()
  "Check ASCII only buffer is unchanged by `properties-encode-buffer'."
  (with-temp-buffer
    (insert "abc=123\ndef=456\nghijkl=foobar\n")
    (properties-encode-buffer)
    (should (equal (buffer-substring (point-min) (point-max))
                   "abc=123\ndef=456\nghijkl=foobar\n"))))

(ert-deftest properties-encode-buffer-with-multibytes ()
  "Check multibyte characters are encoded by `properties-encode-buffer'."
  (with-temp-buffer
    (insert "abc=123\ndef=456\nghijkl=あいう\n")
    (properties-encode-buffer)
    (should (equal (buffer-substring (point-min) (point-max))
                   "abc=123\ndef=456\nghijkl=\\u3042\\u3044\\u3046\n"))))

(ert-deftest properties-encode-buffer-to-uppercase ()
  "Check uppercase characters are used in escape if `properties-unicode-escape-uppercase' is t."
  (with-temp-buffer
    (insert "abc=123\ndef=456\nghijkl=漢字\n")
    (let ((properties-unicode-escape-uppercase t))
      (properties-encode-buffer))
    (should (equal (buffer-substring (point-min) (point-max))
                   "abc=123\ndef=456\nghijkl=\\u6F22\\u5B57\n"))))

(ert-deftest properties-decode-buffer-with-ascii-only ()
  "Check ASCII only buffer is unchanged by `properties-decode-buffer'."
  (with-temp-buffer
    (insert "abc=123\ndef=456\nghijkl=foobar\n")
    (properties-decode-buffer)
    (should (equal (buffer-substring (point-min) (point-max))
                   "abc=123\ndef=456\nghijkl=foobar\n"))))

(ert-deftest properties-decode-buffer-with-multibytes ()
  "Check unicode escaped characters are decoded by `properties-decode-buffer'."
  (with-temp-buffer
    (insert "abc=123\ndef=\\uff14\\uff15\\uFF16\nghijkl = foobar\n")
    (properties-decode-buffer)
    (should (equal (buffer-substring (point-min) (point-max))
                   "abc=123\ndef=４５６\nghijkl = foobar\n"))))

(ert-deftest properties-mode-load-encoded ()
  "Check unicode escaped characters are decoded at load."
  (with-temp-buffer
    (insert "abc=\\u3042\\u3044\\u3046\ndef=\\u3044\\u308d\\u306f\nghi=\\u25cb\\u25b3\\u25a1\n")
    (properties-mode)
    (should (equal (buffer-substring (point-min) (point-max))
                   "abc=あいう\ndef=いろは\nghi=○△□\n"))))

(ert-deftest properties-mode-save-encoded ()
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

(ert-deftest properties-mode-change-mode-with-answer-y ()
  "Check buffer is encoded when changing to other mode if user answers \"y\"."
  (with-temp-buffer
    (let ((s "abc=\\u3042\\u3044\\u3046\ndef=\\u3044\\u308d\\u306f\nghi=\\u25cb\\u25b3\\u25a1\n"))
      (insert s)
      (properties-mode)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_p) t)))
        (conf-mode))
      (should (equal (buffer-substring (point-min) (point-max)) s)))))

(ert-deftest properties-mode-change-mode-with-answer-n ()
  "Check buffer is not encoded when changing to other mode if user answers \"n\"."
  (with-temp-buffer
    (let ((s "abc=\\u3042\\u3044\\u3046\ndef=\\u3044\\u308d\\u306f\nghi=\\u25cb\\u25b3\\u25a1\n"))
      (insert s)
      (properties-mode)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_p) nil)))
        (conf-mode))
      (should (equal (buffer-substring (point-min) (point-max))
                     "abc=あいう\ndef=いろは\nghi=○△□\n")))))

(ert-deftest properties-mode-change-mode-when-ascii-buffer-not-modified-with-answer-y ()
  "Check buffer is not modified if user answers \"y\" and all characters are ASCII when mode changed."
  (with-temp-properties-file
    (insert "abc=123\ndef=456\nghi=789n")
    (save-buffer)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_p) t)))
      (conf-mode))
    (should (not (buffer-modified-p)))))

(ert-deftest properties-mode-change-mode-when-multibytes-buffer-not-modified-with-answer-y ()
  "Check buffer is not modified if user answers \"y\" and buffer contains multibyte characters."
  (with-temp-properties-file
    (insert "abc=あいう\ndef=いろは\nghi=○△□\n")
    (save-buffer)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_p) t)))
      (conf-mode))
    (should (not (buffer-modified-p)))))

(ert-deftest properties-mode-change-mode-when-ascii-buffer-not-modified-with-answer-n ()
  "Check buffer is modified if user answer \"n\" and all characters are ASCII."
  (with-temp-properties-file
    (insert "abc=123\ndef=456\nghi=789n")
    (save-buffer)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_p) nil)))
      (conf-mode))
    (should (not (buffer-modified-p)))))

(ert-deftest properties-mode-change-mode-when-multibytes-buffer-modified-with-answer-n ()
  "Check buffer is modified if user answers \"n\" and buffer contains multibyte characters."
  (with-temp-properties-file
    (insert "abc=あいう\ndef=いろは\nghi=○△□\n")
    (save-buffer)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_p) nil)))
      (conf-mode))
    (should (buffer-modified-p))))
