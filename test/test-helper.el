(require 'cl)
(require 'el-mock)
(require 'undercover)
(require 'subr-x)

(undercover "*.el")

(add-to-list 'load-path ".")
(load "fancy-yank.el")
