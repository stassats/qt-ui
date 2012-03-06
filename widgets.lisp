(in-package #:qt-ui)
(named-readtables:in-readtable :qt)

(defclass clickable-label ()
  ()
  (:metaclass qt-class)
  (:qt-superclass "QLabel")
  (:override ("mouseReleaseEvent" mouse-release-event))
  (:signals ("clicked()")))

(defmethod initialize-instance :after ((instance clickable-label) &key text)
  (if text
      (new instance text)
      (new instance))
  (#_setCursor instance (#_new QCursor (#_Qt::PointingHandCursor))))

(defclass link (clickable-label)
  ((object :initarg :object
           :reader current-object))
  (:metaclass qt-class)
  (:qt-superclass "QLabel")
  (:slots ("viewLink()" %view-link)))

(defmethod initialize-instance :after ((instance link) &key)
  (connect instance "clicked()"
           instance "viewLink()"))

(defgeneric view-link (link object)
  (:method ((link t) (object t))))

(defun %view-link (link)
  (view-link link (current-object link)))

(defmethod mouse-release-event ((clickable-label clickable-label) event)
  (when (enum= (#_Qt::LeftButton)
               (#_button event))
    (emit-signal clickable-label "clicked()")))

(defclass web-link (clickable-label)
  ((url :initarg :url
        :initform nil
        :reader url))
  (:metaclass qt-class)
  (:qt-superclass "QLabel")
  (:slots ("viewLink()" (lambda (link)
                          (print link)
                          (launch-browser (url link))))))

(defmethod initialize-instance :after ((instance web-link) &key)
  (connect instance "clicked()"
           instance "viewLink()"))
