;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ui)
(named-readtables:in-readtable :qt)

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
    (executing-window (result dialog t)
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
    (executing-window (result dialog t)
      (#_text line-edit))))

(defun file-dialog (&key parent
                         (caption "")
                         (directory "")
                         (filter ""))
  (let ((dialog (if parent
                    (#_new QFileDialog parent caption directory filter)
                    (#_new QFileDialog (null-qobject "QWidget")
                           parent caption directory filter))))
    (executing-window (result dialog t)
      (#_selectedFiles dialog))))
