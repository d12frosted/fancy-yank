;;; fancy-yank.el --- apply transformation upon yanking

;; Copyright (c) 2018 Boris Buliga

;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Created: 23 Apr 2018

;; Keywords:
;; Homepage: https://github.com/d12frosted/emacs-fancy-yank

;; Package-Version: 0.0.1
;; Package-Requires: ((emacs "25.1")

;; This file is not part of GNU Emacs.
;;; License: GPLv3

;;; Commentary:
;;
;; Sometimes you just have to transform text from the clipboard before pasting
;; it into the buffer. For example, when inserting links to tickets/issues you
;; might want to format it, so https://github.com/d12frosted/fancy-yank/issue/1
;; becomes
;; [[https://github.com/d12frosted/fancy-yank/issue/1][d12frosted/fancy-yank#1]].
;; Are you doing it manually?
;;
;; In order to use, setup the value of `fancy-yank-rules' and use `fancy-yank'
;; to automatically apply transformations defined by these rules.
;;
;; Example setup:
;;
;; (setq fancy-yank-rules
;; '(("\\(https?://github.com/\\([[:alnum:]-]+\\)/\\([-[:alnum:]]+\\)/[[:alpha:]]+/\\([[:digit:]]+\\).*\\)" . "[[\\1][\\2/\\3#\\4]]")))
;;
;; It will transform GitHub issue/pr link into `org-mode' link with a *fancy* name.
;;

;;; Code:
;;

;;;###autoload
(defvar fancy-yank-rules '()
  "Rules for `fancy-yank-insert'.

This is an association list, where key defines a condition and
value defines transformation.

Condition can come in different flavors.

- If it's a string, then it is used as a regexp passed to
  `string-match-p', and the rule is selected if `string-match-p'
  returns non-nil.

 - If it's a function, then it is called with input passed to it
   and the rule is selected if the function returns non-nil.

Transformation can come in different flavors as well.

- If the condition and the transformation are both strings, then
  the transformation is just a `replace-regexp-in-string'.

- If it's a function, then it is used as transformation.

Simple as that.")

;;;###autoload
(defun fancy-yank ()
  (interactive)
  (fancy-yank-insert (current-kill 0)))

(defun fancy-yank-insert (input)
  (interactive "MInput: ")
  (insert (fy--transform input)))

(defun fy--transform (input)
  (if-let ((rule (fy--find-rule input)))
      (fy--apply rule input)
    input))

(defun fy--find-rule (input)
  (seq-find (lambda (rule)
              (fy--rule-matches-p rule input))
            fancy-yank-rules))

(defun fy--rule-matches-p (rule input)
  (cond ((stringp (car rule))
         (string-match-p (car rule) input))
        ((functionp (car rule))
         (funcall (car rule) input))
        (t nil)))

(defun fy--apply (rule input)
  (cond ((stringp (car rule))
         (replace-regexp-in-string (car rule) (cdr rule) input))
        ((functionp (car rule))
         (funcall (cdr rule) input))
        (t input)))

(provide 'fancy-yank)

;;; fancy-yank.el ends here
