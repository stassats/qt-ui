;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ui)
(named-readtables:in-readtable :qt)

(defun add-widgets (layout &rest widgets)
  (let ((qlayout (find-qclass "QLayout")))
   (dolist (widget widgets)
     (if (qtypep widget qlayout)
         (#_addLayout layout widget)
         (#_addWidget layout widget)))))

(defun new-instance (instance &rest args)
  (if (notany #'null args)
      (apply #'new instance args)
      (new instance)))

(defun dialog-select-item (text list &key
                                       (description #'object-description)
                                       parent)
  (let* ((dialog (if parent
                     (#_new QDialog parent)
                     (#_new QDialog)))
         (vbox (#_new QVBoxLayout dialog))
         (label (#_new QLabel text))
         (combo-box (make-instance 'combo-box
                                   :items list
                                   :description description))
         (buttons (#_new QDialogButtonBox
                         (enum-or
                          (#_QDialogButtonBox::Ok)
                          (#_QDialogButtonBox::Cancel))
                         (#_Qt::Horizontal))))
    (add-widgets vbox
                 label
                 combo-box
                 buttons)

    (connect buttons "accepted()"
             dialog "accept()")
    (connect buttons "accepted()"
             dialog "close()")
    (connect buttons "rejected()"
             dialog "close()")

    (#_exec dialog)
    (when (plusp (#_result dialog))
      (current-item combo-box))))


(defun delete-widgets (layout)
  (loop for item = (#_takeAt layout 0)
        until (null-qobject-p item)
        do
        (let ((layout (#_layout item))
              (widget (#_widget item)))
          (cond ((not (null-qobject-p widget))
                 (#_delete widget))
                ((not (null-qobject-p layout))
                 (delete-widgets layout)))
          (#_delete item))))

(defmacro with-layout ((var name &optional parent-layout) &body body)
  `(let ((,var (optimized-new ,name)))
     ,(when parent-layout
            `(optimized-call t ,parent-layout "addLayout"
                             ,var))
     ,@body))

(defun output-text (layout string &key bold size)
  (let ((label (#_new QLabel string)))
    (when bold
      (#_setBold (#_font label) t))
    (when size
      (#_setPointSize (#_font label) size))
    (#_addWidget layout label)))

(defun exec-window (window parent)
  (if parent
      (#_show window)
      (#_exec window)))

(defun add-qaction (toolbar icon text object signal
                    &key key)
  (let ((action
          (#_addAction toolbar (if (stringp icon)
                                   (#_fromTheme "QIcon" icon)
                                   icon)
                       text
                       object
                       (qsignal signal))))
    (when key
      (#_setShortcut action key))
    action))

(defgeneric object-description (object &key &allow-other-keys)
  (:documentation "Returns one line string to present an object on screen."))

(defmethod object-description (object &key)
  (princ-to-string object))

;;;

(defun add-horizontal-line (layout)
  (let ((frame (#_new QFrame)))
    (#_setFrameStyle frame (#_QFrame::HLine))
    (#_addWidget layout frame)))

;;;

(defgeneric mouse-release-event (ojbect event))

(defgeneric key-press-event (ojbect event))

(defgeneric refresh (widget))
