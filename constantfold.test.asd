#|
  This file is a part of constantfold project.
  Copyright (c) 2019 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage constantfold.test-asd
  (:use :cl :asdf))
(in-package :constantfold.test-asd)


(defsystem constantfold.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of constantfold"
  :license "LLGPL"
  :depends-on (:constantfold
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (test-op :after (op c) (eval (read-from-string "(5am:run! :constantfold)"))
))
