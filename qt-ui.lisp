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

(defmacro new-instance (instance &rest args)
  (if args
      (let ((first-arg (gensym "FIRST")))
        `(let ((,first-arg ,(car args)))
           (if ,first-arg
               (new ,instance ,first-arg ,@(cdr args))
               (new ,instance))))
      `(new ,instance)))

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
                    &key icon key (enabled t))
  (let ((icon (if (stringp icon)
                  (#_QIcon::fromTheme icon)
                  icon))
        (action
          (#_addAction widget
                       text
                       receiver
                       (qsignal signal))))
    (when icon
      (#_setIcon action icon))
    (when key
      (with-objects ((key (#_new QKeySequence key)))
        (#_setShortcut action key)))
    (unless enabled
      (#_setEnabled action nil))
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
    (#_setFrameShape frame (#_QFrame::HLine))
    (#_addWidget layout frame)))

;;;

(defgeneric mouse-press-event (widget event))
(defgeneric mouse-release-event (widget event))
(defgeneric mouse-enter-event (widget event))
(defgeneric mouse-leave-event (widget event))

(defgeneric key-press-event (widget event))
(defgeneric key-release-event (widget event))

(defgeneric focus-in-event (widget event))
(defgeneric focus-out-event (widget event))

(defgeneric paint-event (widget event))

(defgeneric refresh (widget))

;;;

(defgeneric context-menu-event (widget event))
(defgeneric context-menu (widget))

(defmethod context-menu (widget)
  nil)

(defmethod context-menu-event (widget event)
  (let ((menu (context-menu widget)))
    (when menu
      (#_exec menu (#_globalPos event)))))

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
  (with-objects ((key (#_new QKeySequence keys)))
    (if (eq context :window)
        (#_new QShortcut
               key
               parent
               (qslot slot))
        (#_new QShortcut
               key
               parent
               (qslot slot)
               (cffi:null-pointer)
               (shortcut-context-enum context)))))

(defun clipboard-selection ()
  (#_text (#_QApplication::clipboard) (#_QClipboard::Selection)))

(defun clipboard ()
  (#_text (#_QApplication::clipboard)))
