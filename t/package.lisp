#|
  This file is a part of constantfold project.
  Copyright (c) 2019 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :constantfold.test
  (:use :cl
        :constantfold
        :fiveam
        :trivia :alexandria :iterate))
(in-package :constantfold.test)



(def-suite :constantfold)
(in-suite :constantfold)

;; run test with (run! test-name) 

(test constantfold

  )



