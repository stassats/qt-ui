;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ui)
(named-readtables:in-readtable :qt)

(defclass navigable-viewer ()
  ((stack :initform nil
          :accessor stack)
   (back-history :initform nil
                 :accessor back-history)
   (forward-history :initform nil
                    :accessor forward-history)
   (back-button :initform nil
                :accessor back-button)
   (forward-button :initform nil
                   :accessor forward-button)
   (toolbar :initform nil
            :accessor toolbar)
   (page-class :initarg :page-class
                      :initform nil
                      :accessor page-class))
  (:metaclass qt-class)
  (:qt-superclass "QDialog")
  (:slots
   ("back()" back)
   ("forward()" forward)
   ("backMenu(QAction*)" back-menu)
   ("forwardMenu(QAction*)" forward-menu)
   ("refresh()" refresh-viewer)))

(defclass viewer-page ()
  ((object :initarg :object
           :reader current-object))
  (:metaclass qt-class)
  (:qt-superclass "QWidget"))

(defmethod current-object ((viewer navigable-viewer))
  (current-object (current-viewer viewer)))

(defmethod initialize-instance :after ((instance viewer-page) &key parent object)
  (new-instance instance parent)
  (let ((layout (#_new QVBoxLayout)))
    (#_setLayout instance layout)
    (view-object instance object layout)))

(defun current-viewer (viewer)
  (#_currentWidget (stack viewer)))

(defgeneric view-object (viewer object layout))
(defgeneric viewer-object-changed (viewer object))

(defmethod viewer-object-changed (viewer object))

(defun refresh-viewer (viewer)
  (let* ((viewer (current-viewer viewer))
         (layout (#_layout viewer)))
    (delete-widgets layout)
    (view-object viewer (current-object viewer) layout)))

(defmethod initialize-instance :after ((window navigable-viewer) &key parent)
  (new-instance window parent)
  (let ((layout (#_new QVBoxLayout))
        (stack (#_new QStackedLayout))
        (toolbar (#_new QToolBar))
        (back (#_new QToolButton))
        (forward (#_new QToolButton)))
    (setf (stack window) stack
          (back-button window) back
          (forward-button window) forward
          (toolbar window) toolbar)

    (#_setLayout window layout)
    (add-widgets layout toolbar)
    (add-widgets toolbar back forward )
    (add-qaction toolbar "Refresh"
                 window "refresh()"
                 :icon "view-refresh"
                 :key (#_QKeySequence::Refresh))

    (#_setIcon back (#_standardIcon (#_style window) (#_QStyle::SP_ArrowBack)))
    (#_setIcon forward (#_standardIcon (#_style window) (#_QStyle::SP_ArrowForward)))

    (#_setPopupMode back (#_QToolButton::MenuButtonPopup))
    (#_setPopupMode forward (#_QToolButton::MenuButtonPopup))
    (#_setMenu back (#_new QMenu))
    (#_setMenu forward (#_new QMenu))

    (connect (#_menu back) "triggered(QAction*)"
             window "backMenu(QAction*)")
    (connect (#_menu forward) "triggered(QAction*)"
             window "forwardMenu(QAction*)")

    (#_setEnabled back nil)
    (#_setEnabled forward nil)

    (connect back "clicked()" window "back()")
    (connect forward "clicked()" window "forward()")

    (#_addLayout layout stack)))

(defmethod initialize-instance :around ((window navigable-viewer) &key object)
  (call-next-method)
  (set-current-object window object))

(defun set-current-object (window object)
  (let ((viewer-page (make-instance (page-class window)
                                    :parent window
                                    :object object)))
    (add-widgets (stack window) viewer-page)
    (select-widget window viewer-page)))

(defun adjust-menu (window)
  (with-slots (back-history forward-history
               back-button  forward-button)
      window
    (let ((back-menu (#_menu back-button))
          (forward-menu (#_menu forward-button)))
      (#_clear back-menu)
      (#_clear forward-menu)
      (flet ((add-actions (menu history
                                shortcuts)
               (loop for i from 0
                     for navigable-viewer in history
                     for action = (#_new QAction
                                         (object-description
                                          (current-object navigable-viewer))
                                         menu)
                     for first-action = action then first-action
                     do (#_setData action i)
                     (#_addAction menu action)
                     finally (when (and shortcuts first-action)
                               (#_setShortcuts first-action shortcuts)))))
        
        (with-objects ((backspace (#_new QKeySequence :|int| (#_Qt::Key_Backspace)))
                       (back (#_new QKeySequence (#_QKeySequence::Back)))
                       (forward (#_new QKeySequence (#_QKeySequence::Forward))))
            (add-actions back-menu back-history
                         (list backspace back))
         (add-actions forward-menu forward-history
                      (list forward)))))))

(defun adjust-buttons (window)
  (with-slots (back-history forward-history
               back-button  forward-button)
      window
    (#_setEnabled forward-button (consp forward-history))
    (#_setEnabled back-button (consp back-history))
    (adjust-menu window)))

(defun select-widget (window direction &optional (n 0))
  (with-slots (stack
               back-history forward-history)
      window
    (let* ((current (#_currentWidget stack))
          (next
           (case direction
             (:back
              (push current forward-history)
              (let ((backs (subseq back-history n)))
                (psetf back-history (cdr backs)
                       forward-history (append
                                        (reverse (subseq back-history 0 n))
                                        forward-history))
                (car backs)))
             (:forward
              (push current back-history)
              (let ((forths (subseq forward-history n)))
                (psetf forward-history (cdr forths)
                       back-history (append (reverse
                                             (subseq forward-history 0 n))
                                            back-history))
                (car forths)))
             (t
              (unless (eql current direction)
                (push current back-history))
              (setf forward-history nil)
              direction))))
      (#_setCurrentWidget stack next)
      (#_setWindowTitle window (object-description (current-object next)))
      (viewer-object-changed window (current-object next)))
    (adjust-buttons window)))

(defun back (window)
  (select-widget window :back))

(defun forward (window)
  (select-widget window :forward))

(defun back-menu (window action)
  (select-widget window :back (#_data action)))

(defun forward-menu (window action)
  (select-widget window :forward (#_data action)))
