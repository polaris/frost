#|
  This file is a part of frost project.
  Copyright (c) 2012 Jan Deinhard
|#

#|
  A ray tracer in Lisp

  Author: Jan Deinhard
|#

(in-package :cl-user)
(defpackage frost-asd
  (:use :cl :asdf))
(in-package :frost-asd)

(defsystem frost
  :version "0.1-SNAPSHOT"
  :author "Jan Deinhard"
  :license "BSD"
  :depends-on (:png
               :l-math)
  :components ((:module "src"
                :components
                ((:file "frost"))))
  :description "A ray tracer in Lisp"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op frost-test))))
