(in-package :qt-ui)
(named-readtables:in-readtable :qt)

(defvar *qapp* nil)

(defun test ()
  (unless *qapp*
    (setf *qapp* (make-qapplication)))
  (let ((window (make-instance 'list-widget-test)))
    (unwind-protect
         (progn
           (#_show window)
           (#_exec *qapp*))
      (#_hide window))))

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
