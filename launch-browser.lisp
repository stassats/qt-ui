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

(defun launch-browser (url &rest parameters &key &allow-other-keys)
  (let ((url (make-url url parameters)))
    #+sbcl(sb-ext:run-program "browser" (list url)
                              :search t
                              :wait nil)
    #+ccl(ccl:run-program "browser" (list url)
                          :wait nil)))
