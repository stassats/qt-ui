(in-package :qt-ui)
(named-readtables:in-readtable :qt)

(defun test ()
  (with-main-window
      (window (make-instance 'list-widget-test))))

(defclass list-widget-test ()
  ()
  (:metaclass qt-class)
  (:qt-superclass "QDialog")
  (:slots))

(defvar *items* (make-list 20000 :initial-element "A  string"))

(defmethod initialize-instance :after ((window list-widget-test) &key)
  (new window)
  (let ((vbox (#_new QVBoxLayout window))
        (list (time (make-instance 'list-widget :items *items*))))
    (add-widgets vbox list)))
