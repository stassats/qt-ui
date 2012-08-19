(in-package #:qt-ui)
(named-readtables:in-readtable :qt)

(defun launch-browser (url)
  #+sbcl(sb-ext:run-program "opera" (list url)
                            :search t
                            :wait nil)
  #+ccl(ccl:run-program "opera" (list url)
                        :wait nil))
