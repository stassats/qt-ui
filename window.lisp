;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:qt-ui)
(named-readtables:in-readtable :qt)

(defvar *main-window* nil)

(defclass window ()
  ()
  (:metaclass qt-class)
  (:qt-superclass "QWidget"))

#+win32
(defvar *icon-path* (list (namestring (merge-pathnames "icons/" (user-homedir-pathname)))))

(defmethod initialize-instance :before ((window window) &key)
  (make-qapplication)
  #+win32 (#_QIcon::setThemeSearchPaths *icon-path*)
  (#_QIcon::setThemeName "oxygen"))

(defmethod initialize-instance :after ((window window) &key parent
                                                            title)
  (new-instance window parent)
  (when title
    (#_setWindowTitle window title)))

(defgeneric exec-window (window &key modal delete-on-close))

(defmethod exec-window (window &key modal
                                    (delete-on-close t))
  (when delete-on-close
    (#_setAttribute window (#_Qt::WA_DeleteOnClose)))
  (cond ((not *main-window*)
         (#_exec window))
        (modal
         (#_setWindowModality window (#_Qt::WindowModal))
         (#_exec window))
        (t
         (#_show window))))

(defmacro executing-window ((result window &optional modal)
                            &body body)
  "#_exec MODAL or *main-window* is T, #_show otherwise on WINDOW.
Execute body if the result of #_exec is positive."
  (let ((exec (gensym "EXEC"))
        (modal-sym (gensym "MODAL"))
        (window-sym (gensym "WINDOW")))
    `(let* ((,modal-sym ,modal)
            (,exec (or ,modal-sym
                       (not *main-window*)))
            (,window-sym ,window)
            (,result (exec-window ,window-sym
                                  :modal ,modal-sym
                                  :delete-on-close (not ,exec))))
       (declare (ignorable ,result))
       (unwind-protect
            (when (and ,result
                       (plusp ,result))
              (progn ,@body))
         (when ,exec
           (#_delete ,window-sym))))))

