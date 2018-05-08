;;; fancy-yank.el --- apply transformation upon yanking

;; Copyright (c) 2018 Boris Buliga

;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Created: 23 Apr 2018

;; Keywords:
;; Homepage: https://github.com/d12frosted/emacs-fancy-yank

;; Package-Version: 0.0.2
;; Package-Requires: ((emacs "25.1") (org-cliplink "0.2"))

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
;;       '(
;;         ;; transform GitHub issue link to username/repo#number
;;         ("\\(https?://github.com/\\([-[:alnum:]]+\\)/\\([-[:alnum:]]+\\)/[[:alpha:]]+/\\([[:digit:]]+\\)\\).*" .
;;          "[[\\1][\\2/\\3#\\4]]")

;;         ;; the same as before, but much more flexible
;;         ("\\(https?://github.com/\\([-[:alnum:]]+\\)/\\([-[:alnum:]]+\\)/[[:alpha:]]+/\\([[:digit:]]+\\)\\).*" .
;;          (fancy-yank-extract-regex
;;           (lambda (url owner repo number &rest args)
;;             (list url
;;                   (format "%s/%s#%s" owner repo number)))
;;           fancy-yank-format-link))

;;         ;; automatically get the title of web page using `org-cliplink' and
;;         ;; format it acordingly to the current major mode
;;         ("https?://.*" . (fancy-yank-extract-url-title fancy-yank-format-link))

;;         ;; append "FIX " to any other string
;;         (#'identity . (lambda (x) (concat "FIX " x)))))
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

- If it's a list, then every element is treated like a function
  as is called one by one in a pipe manner. Rule itself and the
  input is passed to the first function and it's result is passed
  to the next function and so on. Note that every function must
  return a list, which is passed unpacked to the next function.
  Please see `fancy-yank-extract-regex',
  `fancy-yank-extract-url-title' and `fancy-yank-format-link' for
  examples.

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
  (cond ((and (stringp (car rule))
              (stringp (cdr rule)))
         (replace-regexp-in-string (car rule) (cdr rule) input))
        ((functionp (car rule))
         (funcall (cdr rule) input))
        ((listp (cdr rule))
         (let (value)
           (dolist (f (cdr rule) value)
             (setq value (if (null value)
                             (funcall f rule input)
                           (apply f value))))))
        (t input)))

(defun fancy-yank-extract-regex (rule input)
  "Extract regexp groups from the INPUT as defined in car of the RULE.

Should be used inside the cdr of the RULE."
  (string-match (car rule) input)
  (let ((res '())
        (num 1))
    (while (> num 0)
      (if-let ((x (match-string num input)))
          (progn
            (add-to-list 'res x t)
            (setq num (+ 1 num)))
        (setq num 0)))
    res))

(defun fancy-yank-extract-url-title (rule input &rest args)
  "Get the title of the INPUT url.

Returns a (INPUT, title) list.

Should be used inside the cdr of the RULE."
  (list input
        (org-cliplink-retrieve-title-synchronously input)))

(defun fancy-yank-format-link (url description &rest args)
  "Format link for URL with DESCRIPTION based on current mode.

Should be used inside the cdr of the RULE."
  (cond ((or (eq major-mode 'org-mode)
             (eq major-mode 'org-capture-mode))
         (format "[[%s][%s%s]]"
                 url
                 description
                 (apply #'concat args)))

        ((eq major-mode 'markdown-mode)
         (format "[%s%s](%s)"
                 description
                 (apply #'concat args)
                 url))

        (t (format "%s%s (%s)"
                   description
                   (apply #'concat args)
                   url))))

(provide 'fancy-yank)

;;; fancy-yank.el ends here
