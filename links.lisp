(in-package #:qt-ui)
(named-readtables:in-readtable :qt)

(defclass clickable-label ()
  ((hovered :initarg :hovered
            :initform nil
            :accessor hovered))
  (:metaclass qt-class)
  (:qt-superclass "QLabel")
  (:override ("mouseReleaseEvent" mouse-release-event)
             ("contextMenuEvent" context-menu-event)
             ("enterEvent" mouse-enter-event)
             ("leaveEvent" mouse-leave-event)
             ("paintEvent" paint-event))
  (:signals ("clicked()")))

(defmethod initialize-instance :after ((widget clickable-label) &key text)
  (new-instance widget text)
  (with-objects ((cursor (#_new QCursor (#_Qt::PointingHandCursor))))
    (#_setCursor widget cursor))
  (unless (typep widget 'graphics-link)
    (#_setSizePolicy widget (#_QSizePolicy::Maximum) (#_QSizePolicy::Maximum))
    (#_setMargin widget 2)))

(defmethod mouse-release-event ((clickable-label clickable-label) event)
  (when (enum= (#_Qt::LeftButton)
               (#_button event))
    (emit-signal clickable-label "clicked()")))

(defmethod mouse-enter-event ((widget clickable-label) event)
  (setf (hovered widget) t)
  (#_update widget))

(defmethod mouse-leave-event ((widget clickable-label) event)
  (setf (hovered widget) nil)
  (#_update widget))

(defmethod paint-event ((widget clickable-label) event)
  (cond ((hovered widget)
         (call-next-qmethod)
         (with-objects ((painter (#_new QPainter widget)))
           (with-objects ((rect (#_adjusted (#_rect widget) 1 1 -1 -1)))
             (#_drawRoundedRect painter rect 2 2))))
        (t
         (stop-overriding))))

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
  (view-link link (object link)))

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
