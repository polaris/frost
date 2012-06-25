#|
  This file is a part of frost project.
  Copyright (c) 2012 Jan Deinhard
|#

(in-package :cl-user)
(defpackage frost-test-asd
  (:use :cl :asdf))
(in-package :frost-test-asd)

(defsystem frost-test
  :author "Jan Deinhard"
  :license "BSD"
  :depends-on (:frost
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "frost"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
