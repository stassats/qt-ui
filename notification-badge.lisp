;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ui)
(named-readtables:in-readtable :qt)

(defclass notification-badge ()
  ((text :initarg :text
         :initform nil
         :accessor text))
  (:metaclass qt-class)
  (:qt-superclass "QLabel"))

(defmethod initialize-instance :after ((widget notification-badge)
                                       &key parent text)
  (if parent
      (new widget (princ-to-string text) parent)
      (new widget (princ-to-string text)))
  (#_setFrameStyle widget (#_QFrame::StyledPanel))
  (#_setAutoFillBackground widget t)
  (#_setStyleSheet widget
                   "font: bold; color: white; background-color: red; border-radius: 5px; padding: 0px 3px 0px 3px;"))

(defmethod (setf text) :after (value (widget notification-badge))
  (#_setText widget (princ-to-string value))
  (#_adjustSize widget))
