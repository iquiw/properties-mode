(require 'ert)
(require 'properties-mode)

(ert-deftest properties-encode-buffer-with-ascii-only ()
  (with-temp-buffer
    (insert "abc=123\ndef=456\nghijkl=foobar\n")
    (properties-encode-buffer)
    (should (equal (buffer-substring (point-min) (point-max))
                   "abc=123\ndef=456\nghijkl=foobar\n"))))

(ert-deftest properties-encode-buffer-with-multibytes ()
  (with-temp-buffer
    (insert "abc=123\ndef=456\nghijkl=あいう\n")
    (properties-encode-buffer)
    (should (equal (buffer-substring (point-min) (point-max))
                   "abc=123\ndef=456\nghijkl=\\u3042\\u3044\\u3046\n"))))

(ert-deftest properties-encode-buffer-to-uppercase ()
  (with-temp-buffer
    (insert "abc=123\ndef=456\nghijkl=漢字\n")
    (let ((properties-unicode-escape-uppercase t))
      (properties-encode-buffer))
    (should (equal (buffer-substring (point-min) (point-max))
                   "abc=123\ndef=456\nghijkl=\\u6F22\\u5B57\n"))))

(ert-deftest properties-decode-buffer-with-ascii-only ()
  (with-temp-buffer
    (insert "abc=123\ndef=456\nghijkl=foobar\n")
    (properties-decode-buffer)
    (should (equal (buffer-substring (point-min) (point-max))
                   "abc=123\ndef=456\nghijkl=foobar\n"))))

(ert-deftest properties-decode-buffer-with-multibytes ()
  (with-temp-buffer
    (insert "abc=123\ndef=\\uff14\\uff15\\uFF16\nghijkl = foobar\n")
    (properties-decode-buffer)
    (should (equal (buffer-substring (point-min) (point-max))
                   "abc=123\ndef=４５６\nghijkl = foobar\n"))))

(ert-deftest properties-mode-load-encoded ()
  (with-temp-buffer
    (insert "abc=\\u3042\\u3044\\u3046\ndef=\\u3044\\u308d\\u306f\nghi=\\u25cb\\u25b3\\u25a1\n")
    (properties-mode)
    (should (equal (buffer-substring (point-min) (point-max))
                   "abc=あいう\ndef=いろは\nghi=○△□\n"))))

(ert-deftest properties-mode-save-encoded ()
  (let ((file (make-temp-file "properties-test-" nil ".properties")))
    (unwind-protect
        (progn
          (find-file file)
          (properties-mode)
          (insert "abc=あいう\ndef=いろは\nghi=○△□\n")
          (save-buffer)
          (kill-buffer)
          (with-temp-buffer
            (insert-file-contents file)
            (should (equal (buffer-substring (point-min) (point-max))
                           "abc=\\u3042\\u3044\\u3046\ndef=\\u3044\\u308d\\u306f\nghi=\\u25cb\\u25b3\\u25a1\n"))))
      (delete-file file))))
