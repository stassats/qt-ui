(in-package #:qt-ui)
(named-readtables:in-readtable :qt)

(defclass view-widget ()
  ((model :initform nil
          :accessor model))
  (:metaclass qt-class)
  (:qt-superclass "QWidget"))

(defmethod initialize-instance :after ((widget view-widget)
                                       &key parent
                                            items key link-key
                                            (description #'object-description)
                                            editable
                                            header)
  (new-instance widget parent)
  (let ((model (make-instance 'list-model
                              :items items
                              :key key
                              :link-key link-key
                              :description description
                              :editable editable
                              :header header)))
    (setf (model widget) model)
    (#_setModel widget model)))

(defclass list-model ()
  ((items :initform nil
          :reader items)
   (editable :initform nil
             :initarg :editable))
  (:metaclass qt-class)
  (:slots ("listItemChanged(QStandardItem*)" list-widget-item-changed))
  (:qt-superclass "QStandardItemModel"))

(defgeneric remove-item (item model))

(defgeneric (setf items) (new-items model-or-view &rest args))

(defclass model-item ()
  ((object :initarg :object
           :initform nil
           :accessor object)
   (decoration :initarg :decoration
               :initform nil
               :accessor decoration)
   (children :initarg :children
             :initform nil
             :accessor children)
   (viewable :initarg :viewable
             :initform t
             :accessor viewable)
   (expanded :initarg :expanded
             :initform nil
             :accessor expanded)))

(defun set-header (model header)
  (loop for i from 0
        for header in header
        do
        (#_setHeaderData model i (#_Qt::Horizontal) header)) )

(defmethod initialize-instance :after ((model list-model)
                                       &key parent
                                            items key link-key editable
                                            (description #'object-description)
                                            header)
  (new-instance model parent)
  (when items
    (setf (items model :key key :link-key link-key
                       :description description)
          items))
  (when editable
    (connect model "itemChanged(QStandardItem*)"
             model "listItemChanged(QStandardItem*)"))
  (set-header model header))

(defclass list-widget (view-widget)
  ()
  (:metaclass qt-class)
  (:qt-superclass "QTreeView")
  (:slots
   ("deleteItems()" delete-items)
   ("editObject()" list-widget-edit-object)
   ("editItem()" edit-item)
   ("viewItem(QModelIndex)" list-widget-view-item))
  (:override ("dropEvent" drop-event)
             ("startDrag" start-drag)
             ("dragEnterEvent" drag-enter-event)
             ("dragMoveEvent" drag-move-event)))

(defmethod initialize-instance :after ((widget list-widget)
                                       &key editable expandable
                                            header)
  (connect widget "doubleClicked(QModelIndex)"
           widget "viewItem(QModelIndex)")
  (unless expandable
    (#_setRootIsDecorated widget nil))
  (unless header
    (#_setHeaderHidden widget t))
  (#_setSelectionBehavior widget (#_QAbstractItemView::SelectItems))
  (#_setResizeMode (#_header widget) (#_QHeaderView::ResizeToContents))
  (#_setEditTriggers widget (#_QAbstractItemView::NoEditTriggers))
  (when editable
    (#_setDefaultDropAction widget (#_Qt::MoveAction))
    (#_setDragDropMode widget (#_QAbstractItemView::InternalMove))))

;;;

(defgeneric view-item (list-widget item)
  (:method ((list-widget t) (item t))))

(defgeneric edit-object (list-widget item)
  (:method ((list-widget t) (item t))))

;;;

(defun clear-list-model (model)
  (setf (slot-value model 'items) nil)
  (#_clear model))

(defmethod items ((widget view-widget))
  (items (model widget)))

(defmethod (setf items) (items (widget view-widget)
                         &rest args &key &allow-other-keys)
  (apply '(setf items) items
         (model widget) args)
  (set-expanded widget))

(defmethod (setf items) (items (model list-model)
                                &rest args &key &allow-other-keys)
  (clear-list-model model)
  (apply 'list-append model items args))

(defmethod list-append ((widget view-widget) items
                        &rest args &key &allow-other-keys)
  (apply 'list-append (model widget) items args))

(defun %make-item (object decoration description editable)
  (let ((item (#_new QStandardItem (funcall description object))))
    (when decoration
      (#_setData item decoration (#_Qt::DecorationRole)))
    (when editable
      (#_setFlags item (logxor (#_flags item)
                               (primitive-value
                                (#_Qt::ItemIsDropEnabled)))))
    item))

(defun add-item-children (model-item parent editable description)
  (loop for object in (children model-item)
        for row = (alexandria:ensure-list object)
        for row-number from 0
        nconc (loop for column-number from 0
                    for object in row
                    for item = (make-item object description editable)
                    do
                    (#_setChild parent row-number column-number item)
                    collect item)))

(defun make-item (object description editable)
  (if (typep object 'model-item)
      (let ((item (%make-item (object object) (decoration object)
                   description editable)))
        (when (children object)
          (add-item-children object item editable description))
        item)
      (%make-item object nil description editable)))

(defmethod list-append ((model list-model) items
                        &key key link-key (description #'object-description))
  (when items
    (with-slots (editable (current-items items)) model
      (let ((processed-items (if key
                                 (mapcar key items)
                                 (copy-tree items)))
            (current-row-number (length current-items)))
        (setf current-items
              (append current-items
                      (if link-key
                          (mapcar link-key processed-items)
                          processed-items)))
        (loop for object in processed-items
              for row = (alexandria:ensure-list object)
              for row-number from current-row-number
              nconc (loop for column-number from 0
                          for object in row
                          for item = (make-item object description editable)
                          do (#_setItem model row-number column-number
                                        item)
                          collect item))))))

(defmethod remove-item (item (view view-widget))
  (remove-item item (model view)))

(defmethod remove-item (item (model list-model))
  (with-slots (items) model
    (let ((n (if (integerp item)
                 item
                 (#_row item))))
      (setf items (remove-if (constantly t) items
                               :start n
                               :count 1))
      (#_removeRow model n))))

(defun model-index-tree-address (model-index)
  (let (result)
    (loop for index = model-index then (#_parent index)
          for row = (#_row index)
          until (= row -1)
          do (push (cons row (#_column index)) result))
    result))

(defun access-row-item (items index)
  (destructuring-bind (row . column) index
   (let ((row (nth row items)))
     (cond ((consp row)
            (nth column row))
           ((zerop column)
            row)
           (t
            (error "Wrong column"))))))

(defun access-model-item (items model-index)
  (loop for index in (model-index-tree-address model-index)
        for object = (access-row-item items index)
        then (access-row-item children index)
        for children = (and (typep object 'model-item)
                            (children object))
        finally (return object)))

(defun item-from-model-index (model-index widget)
  (access-model-item (items widget) model-index))

(defun list-widget-view-item (list-widget item)
  (let ((item (print (item-from-model-index item list-widget))))
    (typecase item
      (model-item
       (when (viewable item)
         (view-item list-widget (object item))))
      (t
       (view-item list-widget item)))))

(defun list-widget-edit-object (list-widget)
  (let ((item (single-selected list-widget)))
    (when item
      (edit-object list-widget item))))

(defun list-widget-item-changed (widget item)
  (let ((list-item (nthcdr (#_row item) (items widget))))
    (when (stringp (car list-item))
      (setf (car list-item) (#_text item)))))

(defun selected-items (list-widget)
  (loop for item in (#_selectedIndexes list-widget)
        collect (item-from-model-index item list-widget)))

(defun selected-rows (list-widget)
  (loop for item in (#_selectedIndexes list-widget)
        collect (nth (#_row item)
                     (items list-widget))))

(defun delete-items (list-widget)
  (loop for item in (nreverse (#_selectedIndexes list-widget))
        do (remove-item item list-widget)))

(defun single-selected (list-widget)
  (let ((items (selected-items list-widget)))
    (when (= (length items) 1)
      (car items))))

(defun scroll-to-item (widget item)
  (#_setCurrentIndex widget (#_indexFromItem (model widget) item)))



(defun add-item (list-widget)
  (let ((item (car (list-append list-widget '("")))))
    (scroll-to-item list-widget item)
    (edit-item list-widget)))

(defun edit-item (list-widget)
  (let ((item (#_currentIndex list-widget)))
    (#_edit list-widget item)))

;;;

(defmethod start-drag ((list list-widget) actions)
  ;; (let ((selected (#_selectedIndexes list))
  ;;       (drag (#_new QDrag list)))
  ;;   (#_setMimeData drag (#_new QMimeData))
  ;;   (#_exec drag actions))
  )

(defmethod drag-enter-event ((list list-widget) event)
  (when (eql list (#_source event))
    (#_acceptProposedAction event)
    (#_setState list (#_QAbstractItemView::DraggingState))))

(defmethod drag-move-event ((list list-widget) event)
  (#_acceptProposedAction event))

(defmethod drop-event ((list list-widget) event)
  (print event)

  ;; (#_accept event)
  )

;;;

(defclass combo-box (view-widget)
  ()
  (:metaclass qt-class)
  (:qt-superclass "QComboBox"))

(defmethod initialize-instance :after ((widget combo-box)
                                       &key items current-item)
  (#_setCurrentIndex widget (or (position current-item items) 0)))

(defun current-item (combo-box)
  (let ((index (#_currentIndex combo-box)))
    (unless (minusp index)
      (nth index (items combo-box)))))

(defun %walk-model (function items)
  (loop for item in items
        for row = (alexandria:ensure-list item)
        for row-number from 0
        do
        (loop for column-number from 0
              for item in row
              collect (funcall function item (cons row-number column-number)))))

(defun walk-model (function model)
  (%walk-model function (items model)))

(defun set-expanded (list-widget)
  (let ((model (model list-widget)))
    (walk-model
     (lambda (item index)
       (when (and (typep item 'model-item)
                  (expanded item))
         (#_expand list-widget
                   (#_index model (car index) (cdr index)))))

     model)))
