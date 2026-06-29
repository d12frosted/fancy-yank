(require 'cl)
(require 'el-mock)
(require 'undercover)
(require 'subr-x)
;; Load `org' before `ol' autoloads fire.  On modern Org, loading `ol'
;; on its own runs `org-link-descriptive's :set handler, which calls
;; `org-restart-font-lock' from `org' and errors out if it isn't loaded.
(require 'org)

;; Pin the coverage report format.  In CI there is no Coveralls token and
;; we run in batch, so undercover's auto-detection returns nil and errors
;; with "Report format not configured".  Print a plain text report instead.
(undercover "*.el"
            (:report-format 'text)
            (:send-report nil))

(add-to-list 'load-path ".")
(load "fancy-yank.el")
