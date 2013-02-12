;;; -*- Mode: Lisp -*-

(defsystem qt-ui
  :serial t
  :depends-on (qt)
  :components ((:file "packages")
               (:file "qt-ui")
               (:file "window")
               (:file "dialogs")
               (:file "navigable-viewer")
               (:file "launch-browser")
               (:file "links")
               (:file "list-widget")))
