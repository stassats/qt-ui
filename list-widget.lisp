(in-package #:qt-ui)
(named-readtables:in-readtable :qt)

(defclass view-widget ()
  ((model :initform nil
          :accessor model)
   (proxy-model :initarg :proxy-model
                :initform nil
                :accessor proxy-model)
   (expandable :initarg :expandable
               :initform nil
               :accessor expandable))
  (:metaclass qt-class)
  (:qt-superclass "QWidget"))

(defmethod initialize-instance :after ((widget view-widget)
                                       &key parent
                                            items key row-key
                                            (description #'object-description)
                                            editable
                                            header)
  (new-instance widget parent)
  (let ((model (make-instance 'list-model
                              :items items
                              :parent widget
                              :key key
                              :row-key row-key
                              :description description
                              :editable editable
                              :header header)))
    (setf (model widget) model)
    (#_setModel widget model)))

(defclass list-model ()
  ((items :initform nil
          :reader items)
   (editable :initform nil
             :initarg :editable)
   (header :initarg :header
           :initform nil
           :accessor header)
   (key :initarg :key
        :initform nil
        :accessor key)
   (row-key :initarg :row-key
            :initform nil
            :accessor row-key)
   (description :initarg :description
                :initform nil
                :accessor description))
  (:metaclass qt-class)
  (:qt-superclass "QStandardItemModel")
  (:slots ("listItemChanged(QStandardItem*)" list-widget-item-changed)))

(defmethod initialize-instance :after ((model list-model)
                                       &key parent
                                            items key row-key editable
                                            description)
  (new-instance model parent)
  (when items
    (setf (items model :key key
                       :row-key row-key
                       :description description)
          items))
  (when editable
    (connect model "itemChanged(QStandardItem*)"
             model "listItemChanged(QStandardItem*)")))

(defclass model-item ()
  ((object :initarg :object
           :initform nil
           :accessor object)
   (text :initarg :text
         :initform #'object-description
         :accessor text)
   (tool-tip :initarg :tool-tip
             :initform nil
             :accessor tool-tip) 
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
        (#_setHeaderData model i (#_Qt::Horizontal) header)))

(defclass proxy-model ()
  ((model :initarg :model
          :initform nil
          :accessor model)
   (filter-function :initarg :filter-function
                    :initform nil
                    :accessor filter-function))
  (:metaclass qt-class)
  (:qt-superclass "QSortFilterProxyModel")
  (:override ("filterAcceptsRow" filter-accepts-row)))

(defmethod initialize-instance :after ((widget proxy-model) &key parent
                                                                  model)
  (new-instance widget parent)
  (#_setDynamicSortFilter widget t)
  (#_setSourceModel widget model)
  (setf (model widget) model))

(defgeneric filter-accepts-row (proxy row parent))

(defmethod filter-accepts-row ((proxy proxy-model) row parent)
  (when (or (not (filter-function proxy))
            (funcall (filter-function proxy) (nth row (items (model proxy)))))
    (stop-overriding)))

(defclass list-widget (view-widget)
  ((selection-behavior :initarg :selection-behavior
                       :initform nil
                       :accessor selection-behavior)
   (sorting :initarg :sorting
            :initform nil
            :accessor sorting))
  (:metaclass qt-class)
  (:qt-superclass "QTreeView")
  (:slots
   ("deleteItems()" delete-items)
   ("editObject()" list-widget-edit-object)
   ("editItem()" edit-item)
   ("viewItem(QModelIndex)" list-widget-view-item)
   ("displayMenu(QPoint)" list-widget-display-menu))
  (:override ;; ("dropEvent" drop-event)
   ;; ("startDrag" start-drag)
   ;; ("dragEnterEvent" drag-enter-event)
   ;; ("dragMoveEvent" drag-move-event)
   ("keyPressEvent" key-press-event))
  (:signals ("returnPressed()")))

(defun display-model (widget)
  (or (proxy-model widget)
      (model widget)))

(defmethod initialize-instance :after ((widget list-widget)
                                       &key editable expandable
                                            header
                                            (selection-behavior :items)
                                            (selection-mode :single)
                                            sorting
                                            filter-function)
  (connect widget "doubleClicked(QModelIndex)"
           widget "viewItem(QModelIndex)")
  (if expandable
      (set-expanded widget)
      (#_setRootIsDecorated widget nil))
  (unless header
    (#_setHeaderHidden widget t))
  (set-selection-behavior widget selection-behavior)
  (set-selection-mode widget selection-mode)
  (#_setResizeMode (#_header widget) (#_QHeaderView::ResizeToContents))
  (#_setEditTriggers widget (#_QAbstractItemView::NoEditTriggers))
  (#_setContextMenuPolicy widget (#_Qt::CustomContextMenu))
  (connect widget "customContextMenuRequested(QPoint)"
           widget "displayMenu(const QPoint &)")
  (when editable
    (#_setDefaultDropAction widget (#_Qt::MoveAction))
    (#_setDragDropMode widget (#_QAbstractItemView::InternalMove)))
  (when sorting
    (let ((proxy-model (make-instance 'proxy-model
                                      :model (model widget)
                                      :parent widget
                                      :filter-function filter-function)))
      (setf (proxy-model widget) proxy-model)      
      (#_setModel widget proxy-model)
      (#_setSortingEnabled widget t))))

(defun set-selection-behavior (list-widget behavior)
  (case behavior
    (:rows
     (#_setAllColumnsShowFocus list-widget t)))
  (unless (eql (selection-behavior list-widget) behavior)
    (#_setSelectionBehavior
     list-widget
     (ecase behavior
       (:items (#_QAbstractItemView::SelectItems))
       (:rows
        (#_setAllColumnsShowFocus list-widget t)
        (#_QAbstractItemView::SelectRows))
       (:columns (#_QAbstractItemView::SelectColumns))))
    (setf (selection-behavior list-widget) behavior)))

(defun set-selection-mode (list-widget mode)
  (#_setSelectionMode list-widget
                      (ecase mode
                        (:single (#_QAbstractItemView::SingleSelection))
                        (:contiguous (#_QAbstractItemView::ContiguousSelection))
                        (:extended (#_QAbstractItemView::ExtendedSelection))
                        (:multi (#_QAbstractItemView::MultiSelection))
                        (:no (#_QAbstractItemView::NoSelection)))))

;;;

(defgeneric view-item (list-widget item)
  (:method ((list-widget t) (item t))))

(defgeneric edit-object (list-widget item)
  (:method ((list-widget t) (item t))))

(defgeneric display-menu (list-widget item)
  (:method ((list-widget t) (item t))))

(defgeneric display-menu-multiple-rows (list-widget items)
  (:method ((list-widget t) (items t))))

(defgeneric display-menu-no-rows (list-widget)
  (:method ((list-widget t))))

(defgeneric remove-item (item model))

(defgeneric (setf items) (new-items model-or-view &rest args))

(defgeneric current-index (view-widget))
(defgeneric (setf current-index) (row view-widget &optional column))

;;;

(defun clear-list-model (model)
  (setf (slot-value model 'items) nil)
  (#_clear model))

(defmethod items ((widget view-widget))
  (items (model widget)))

(defmethod (setf items) (items (widget view-widget)
                         &rest args &key &allow-other-keys)
  (apply #'(setf items) items
         (model widget) args)
  (when (expandable widget)
    (set-expanded widget)))

(defmethod (setf items) (items (model list-model)
                                &rest args &key &allow-other-keys)
  (clear-list-model model)
  (apply 'list-append model items args))

(defgeneric list-append (widget items &rest args))

(defmethod list-append ((widget view-widget) items
                        &rest args &key &allow-other-keys)
  (apply 'list-append (model widget) items args))

(defun %make-item (object description editable
                   &optional model-item)
  (let ((item (if description
                  (#_new QStandardItem (etypecase description
                                         (function
                                          (funcall description object))
                                         (string description)))
                  (#_new QStandardItem))))
    (when model-item
      (with-slots (decoration tool-tip) model-item
        (when decoration
          (#_setData item decoration (#_Qt::DecorationRole)))
        (when tool-tip
          (#_setData item (object-description tool-tip) (#_Qt::ToolTipRole)))))
    (when editable
      (#_setFlags item (logxor (#_flags item)
                               (primitive-value
                                (#_Qt::ItemIsDropEnabled)))))
    item))

(defun add-item-children (model-item parent editable description)
  (loop for row in (children model-item)
        for row-number from 0
        do
        (if (consp row)
            (loop for column-number from 0
                  for object in row
                  for item = (make-item object description editable)
                  for x =
                  (#_setChild parent row-number item)
                  then
                  (#_setChild parent row-number column-number item))
            (#_setChild parent row-number
                        (make-item row description editable)))))

(defun make-item (object description editable)
  (if (typep object 'model-item)
      (let ((item (%make-item (object object) (text object) editable
                              object)))
        (when (children object)
          (add-item-children object item editable description))
        item)
      (%make-item object description editable)))

(defun lay-items (model items
                  &key (start 0) key row-key description)
  (with-slots (editable (current-items items)) model
    (let ((key (or key (key model)
                   #'identity))
          (row-key (or row-key (row-key model)
                       #'identity))
          (description (or description (description model)
                           #'object-description)))
      (emit-signal model "layoutAboutToBeChanged()")
      (with-signals-blocked (model)
        (loop for x in items
              for row = (funcall row-key x)
              for row-number from start
              do
              (if (consp row)
                  (loop for column-number from 0
                        for object in row
                        for item = (make-item (funcall key object)
                                              description editable)
                        for x =
                        (#_setItem model row-number item)
                        then
                        (#_setItem model row-number column-number item))
                  (#_setItem model row-number
                             (make-item (funcall key row)
                                        description editable)))))
      (emit-signal model "layoutChanged()"))))

(defmethod list-append ((model list-model) items
                        &key key row-key description)
  (when items
    (lay-items model items
               :start (length (items model))
               :key key
               :row-key row-key
               :description description)
    (setf (slot-value model 'items)
          (append (items model) items))
    (when (header model)
      (set-header model (header model)))))

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

(defun make-tree-model-index (model path)
  (loop for (row . column) in path 
        for index = (#_index model row column)
        then (#_index model row column index)
        finally (return index)))

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

(defun map-to-source (model-index widget)
  (if (proxy-model widget)
      (#_mapToSource (proxy-model widget) model-index)
      model-index))

(defun item-from-model-index (model-index widget)
  (ecase (selection-behavior widget)
    (:items (access-model-item (items widget) model-index))
    (:rows (nth (#_row model-index) (items widget)))))

(defun object-from-item (item)
  (typecase item
      (model-item
       (when (viewable item)
         (object item)))
      (t
       item)))

(defun list-widget-view-item (list-widget item)
  (let ((object (object-from-item (item-from-model-index
                                   (map-to-source item list-widget)
                                   list-widget))))
    (when object
      (view-item list-widget object))))

(defun list-widget-edit-object (list-widget)
  (let ((item (single-selected list-widget)))
    (when item
      (edit-object list-widget item))))

(defun list-widget-item-changed (widget item)
  (let ((list-item (nthcdr (#_row item) (items widget))))
    (when (stringp (car list-item))
      (setf (car list-item) (#_text item)))))

(defun list-widget-display-menu (widget point)
  (let* ((selected (selected-items widget))
         (length (length selected))
         (menu (cond ((= length 1)
                      (display-menu widget (car selected)))
                     ((> length 1)
                      (display-menu-multiple-rows widget selected))
                     (t
                      (display-menu-no-rows widget)))))
    (when menu
      (#_exec menu (#_mapToGlobal widget point)))))

(defun %selected-indexes (list-widget)
  (if (proxy-model list-widget)
      (#_indexes (#_mapSelectionToSource
                  (proxy-model list-widget)
                  (#_selection (#_selectionModel list-widget))))
      (#_selectedIndexes list-widget)))

(defun selected-indexes (list-widget)
  (let ((indexes (%selected-indexes list-widget)))
    (ecase (selection-behavior list-widget)
      (:items
       indexes)
      (:rows
       (loop for index in indexes
             for previous = -1 then row
             for row = (#_row index)
             when (/= previous row)
             collect index)))))

(defun selected-items (list-widget)
  (loop for index in (selected-indexes list-widget)
        collect (object-from-item (item-from-model-index index list-widget))))

(defun selected-rows (list-widget)
  (loop for item in (selected-indexes list-widget)
        collect (nth (#_row item)
                     (items list-widget))))

(defun delete-items (list-widget)
  (loop for item in (selected-indexes list-widget)
        do (remove-item item list-widget)))

(defun single-selected (list-widget)
  (let ((items (selected-items list-widget)))
    (when (= (length items) 1)
      (car items))))

(defmethod current-tree-index ((list-widget list-widget))
  (model-index-tree-address (#_currentIndex list-widget)))

(defmethod (setf current-tree-index) (path (list-widget list-widget))
  (#_setCurrentIndex list-widget (make-tree-model-index (model list-widget) path)))

(defmethod current-index ((list-widget list-widget))
  (let ((index (#_currentIndex list-widget)))
    (values (#_row index) (#_column index))))

(defmethod (setf current-index) (row (list-widget list-widget)
                                  &optional (column 0))
  (#_setCurrentIndex list-widget
                     (#_index (display-model list-widget) row column)))

(defun move-to-row (widget index)
  (#_setCurrentIndex widget index)
  (#_scrollTo widget index))

(defun previous-row (list-widget)
  (#_indexAbove list-widget (#_currentIndex list-widget)))

(defun next-row (list-widget)
  (#_indexBelow list-widget (#_currentIndex list-widget)))

(defun move-to-previous-row (list-widget)
  (move-to-row list-widget (previous-row list-widget)))

(defun move-to-next-row (list-widget)
  (move-to-row list-widget (next-row list-widget)))

(defun add-item (list-widget)
  (let ((end (length (items list-widget))))
    (list-append list-widget '(""))
    (setf (current-index list-widget) end)
    (edit-item list-widget)))

(defun edit-item (list-widget)
  (#_edit list-widget (#_currentIndex list-widget)))

(defmethod refresh ((widget list-widget))
  (multiple-value-bind (row column) (current-index widget)
    (lay-items (model widget) (items widget))
    (setf (current-index widget column) row)))

(defmethod key-press-event ((widget list-widget) event)
  (let ((key (#_key event)))
    (when (or (= key (primitive-value (#_Qt::Key_Return)))
              (= key (primitive-value (#_Qt::Key_Enter))))
      (emit-signal widget "returnPressed()")))
  (stop-overriding))

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

(defmethod current-index ((combo-box combo-box))
  (#_currentIndex combo-box))

(defmethod (setf current-index) (row (combo-box combo-box)
                                 &optional column)
  (declare (ignore column))
  (#_setCurrentIndex combo-box row))

(defmethod initialize-instance :after ((widget combo-box)
                                       &key items (current-item nil current-item-p))
  (setf (current-index widget)
        (or (and current-item-p
                 (position current-item items)) 0)))

(defun current-item (combo-box)
  (let ((index (#_currentIndex combo-box)))
    (unless (minusp index)
      (nth index (items combo-box)))))

;;;

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
  (let ((display-model (display-model list-widget)))
    (walk-model
     (lambda (item index)
       (when (and (typep item 'model-item)
                  (expanded item))
         (#_expand list-widget
                   (#_index display-model (car index) (cdr index)))))
     (model list-widget))))
