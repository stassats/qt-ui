(in-package #:qt-ui)
(named-readtables:in-readtable :qt)

(defun make-url (url parameters)
  (with-output-to-string (str)
    (write-string url str)
    (loop for delimiter = #\? then #\&
          for (key value) on parameters by #'cddr
          do
          (write-char delimiter str)
          (write-string (drakma:url-encode (if (symbolp key)
                                               (string-downcase key)
                                               key)
                                           :utf-8)
                        str)
          (write-char #\= str)
          (write-string (drakma:url-encode value :utf-8) str))))

(defun run-program (program &rest args)
  #+sbcl(sb-ext:run-program program args
                            :search t
                            :wait nil)
  #+ccl(ccl:run-program program args
                        :wait nil))

(defun launch-browser (url &rest parameters &key &allow-other-keys)
  (run-program #+darwin "open"
               #-darwin "browser"
               (make-url url parameters)))
