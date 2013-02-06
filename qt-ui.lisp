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
        `(#_addLayout ,parent-layout ,var))
     ,@body))

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

(defgeneric mouse-press-event (object event))
(defgeneric mouse-release-event (ojbect event))

(defgeneric key-press-event (ojbect event))
(defgeneric key-release-event (ojbect event))

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
