;;; -*- Mode: Lisp -*-

(defsystem qt-ui
  :serial t
  :depends-on (qt)
  :components ((:file "packages")
               (:file "qt-ui")
               (:file "dialogs")
               (:file "navigable-viewer")
               (:file "launch-browser")
               (:file "links")
               (:file "list-widget")
               (:file "window")))
