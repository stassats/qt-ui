(in-package #:qt-ui)
(named-readtables:in-readtable :qt)

(defclass clickable-label ()
  ()
  (:metaclass qt-class)
  (:qt-superclass "QLabel")
  (:override ("mouseReleaseEvent" mouse-release-event)
             ("contextMenuEvent" context-menu-event))
  (:signals ("clicked()")))

(defmethod mouse-release-event ((clickable-label clickable-label) event)
  (when (enum= (#_Qt::LeftButton)
               (#_button event))
    (emit-signal clickable-label "clicked()")))

(defmethod initialize-instance :after ((instance clickable-label) &key text)
  (new-instance instance text)
  (with-objects ((cursor (#_new QCursor (#_Qt::PointingHandCursor))))
    (#_setCursor instance cursor)))

;;;

(defclass link (clickable-label)
  ((object :initarg :object
           :reader object))
  (:metaclass qt-class)
  (:slots ("viewLink()" %view-link)))

(defmethod initialize-instance :after ((instance link) &key)
  (connect instance "clicked()"
           instance "viewLink()"))

(defgeneric view-link (link object)
  (:method ((link t) (object t))))

(defun %view-link (link)
  (view-link link (current-object link)))

;;;

(defclass graphics-link (link)
  ()
  (:metaclass qt-class)
  (:qt-superclass "QGraphicsTextItem")
  (:override ("mousePressEvent" mouse-press-event)))

;;; Needs to override, otherwise no other events will be delivered
(defmethod mouse-press-event ((link graphics-link) event)
  (declare (ignore event)))

;;;

(defclass web-link (link)
  ((object :initarg :url
           :reader url))
  (:metaclass qt-class))

(defmethod view-link ((link web-link) url)
  (launch-browser url))
