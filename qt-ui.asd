;;; -*- Mode: Lisp -*-

(defsystem qt-ui
  :serial t
  :depends-on (qt)
  :components ((:file "packages")
               (:file "qt-ui")
               (:file "list-widget")))
