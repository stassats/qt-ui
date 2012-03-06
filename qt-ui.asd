;;; -*- Mode: Lisp -*-

(defsystem qt-ui
  :serial t
  :depends-on (qt)
  :components ((:file "packages")
               (:file "qt-ui")
               (:file "launch-browser")
               (:file "widgets")
               (:file "list-widget")))
