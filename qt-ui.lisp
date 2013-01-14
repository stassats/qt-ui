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

(defmacro with-layout ((var name &optional parent-layout) &body body)
  `(let ((,var (optimized-new ,name)))
     ,(when parent-layout
        `(optimized-call t ,parent-layout "addLayout"
                         ,var))
     ,@body))

(defun add-dialog-buttons (widget layout &key default-ok)
  (with-layout (hbox "QHBoxLayout" layout)
    (let ((ok (#_new QPushButton
                     (#_standardIcon (#_style widget)
                                     (#_QStyle::SP_DialogOkButton))
                     "Ok"))
          (cancel (#_new QPushButton
                         (#_standardIcon (#_style widget)
                                         (#_QStyle::SP_DialogCancelButton))
                         "Cancel")))
      (#_addStretch hbox)
      (add-widgets hbox ok cancel)

      (unless default-ok
        (#_setAutoDefault ok nil))
      (#_setAutoDefault cancel nil)

      (connect ok "clicked()"
               widget "accept()")
      (connect cancel "clicked()"
               widget "close()")
      (connect ok "clicked()"
               widget "accept()")
      (values ok cancel))))

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
                                   :description description)))
    (add-widgets vbox label combo-box)
    (add-dialog-buttons dialog vbox :default-ok t)

    (when (plusp (#_exec dialog))
      (current-item combo-box))))

(defun input-line-dialog (&key prompt parent
                               initial-text)
  (let* ((dialog (if parent
                     (#_new QDialog parent)
                     (#_new QDialog)))
         (vbox (#_new QVBoxLayout dialog))
         (label (and prompt
                     (#_new QLabel prompt)))
         (line-edit (if initial-text
                        (#_new QLineEdit initial-text)
                        (#_new QLineEdit))))
    (when label
      (add-widgets vbox label))
    (add-widgets vbox line-edit)
    (add-dialog-buttons dialog vbox :default-ok t)
    (#_addStretch vbox)

    (when (plusp (#_exec dialog))
      (#_text line-edit))))

;;;

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

(defun output-text (layout string &key bold size)
  (let ((label (#_new QLabel string)))
    (when bold
      (#_setBold (#_font label) t))
    (when size
      (#_setPointSize (#_font label) size))
    (#_addWidget layout label)))

(defun add-qaction (widget text receiver signal
                    &key icon key)
  (let ((icon (if (stringp icon)
                  (#_fromTheme "QIcon" icon)
                  icon))
        (action
          (#_addAction widget 
                       text
                       receiver
                       (qsignal signal))))
    (when icon
      (#_setIcon action icon))
    (when key
      (#_setShortcut action
                     (#_new QKeySequence key)))
    action))

(defgeneric object-description (object &key &allow-other-keys)
  (:documentation "Returns one line string to present an object on screen."))

(defmethod object-description :around (object &key)
  (let (*print-pretty*)
    (call-next-method)))

(defmethod object-description (object &key)
  (if (stringp object)
      object
      (princ-to-string object)))

;;;

(defun add-horizontal-line (layout)
  (let ((frame (#_new QFrame)))
    (#_setFrameStyle frame (#_QFrame::HLine))
    (#_addWidget layout frame)))

;;;

(defgeneric mouse-release-event (ojbect event))
(defgeneric mouse-press-event (object event))

(defgeneric key-press-event (ojbect event))

(defgeneric refresh (widget))

;;;

(defun shortcut-context-enum (context)
  (ecase context
    (:widget
     (#_Qt::WidgetShortcut))
    (:widget-with-children
     (#_Qt::WidgetWithChildrenShortcut))
    (:window
     (#_Qt::WindowShortcut))
    (:application
     (#_Qt::WidgetWithChildrenShortcut))))

(defun make-shortcut (parent keys slot &key (context :window))
  (if (eq context :window)
      (#_new QShortcut
             (#_new QKeySequence keys)
             parent
             (qslot slot))
      (#_new QShortcut
             (#_new QKeySequence keys)
             parent
             (qslot slot)
             (cffi:null-pointer)
             (shortcut-context-enum context))))

(defun clipboard-selection ()
  (#_text (#_QApplication::clipboard) (#_QClipboard::Selection)))

(defun clipboard ()
  (#_text (#_QApplication::clipboard)))
