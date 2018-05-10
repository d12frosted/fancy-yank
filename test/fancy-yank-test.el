(ert-deftest fancy-yank-empty-rules-test ()
  "Tests that `fancy-yank' works as expected without any rules."
  (should (equal "some-random-string"
                 (fy--transform "some-random-string"))))

(ert-deftest fancy-yank-simple-regexp-test ()
  "Tests `fancy-yank' with simple regexp rules."
  (let ((fancy-yank-rules
         '(("The \\(quick\\) brown fox jumps over the \\(lazy\\) dog" . "\\1 \\2")
           ("ftp://\\([a-z]+\\).*" . "\\1"))))
    (should (equal "quick lazy"
                   (fy--transform "The quick brown fox jumps over the lazy dog")))
    (should (equal "random"
                   (fy--transform "ftp://random.org")))
    (should (equal "some-random-string"
                   (fy--transform "some-random-string")))))

(ert-deftest fancy-yank-extract-regexp-test ()
  "Tests `fancy-yank-extract-regexp'."
  (let ((fancy-yank-rules
         '(("The \\(quick\\) brown fox jumps over the \\(lazy\\) dog"
            . (fancy-yank-extract-regex
               (lambda (one two &rest args)
                 (concat one " " two args)))))))
    (should (equal "quick lazy"
                   (fy--transform "The quick brown fox jumps over the lazy dog")))
    (should (equal "some-random-string"
                   (fy--transform "some-random-string")))))

(ert-deftest fancy-yank-format-link-test ()
  "Tests `fancy-yank-extract-regexp'."
  (let ((major-mode 'org-mode))
    (should (equal "[[https://google.com][goOgl]]"
                   (fancy-yank-format-link "https://google.com" "goOgl")))))

(ert-deftest fancy-yank-extract-url-title-test ()
  "Tests `fancy-yank-extract-regexp'."
  (with-mock
    (stub org-cliplink-retrieve-title-synchronously => "goOgl")
    (let ((fancy-yank-rules
           '(("\\(https?://.*\\)"
              . (fancy-yank-extract-url-title)))))
      (should (equal '("https://google.com" "goOgl")
                     (fy--transform "https://google.com")))
      (should (equal "some-random-string"
                     (fy--transform "some-random-string"))))))
