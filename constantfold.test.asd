#|

This file is a part of CONSTANTFOLD project.
Copyright (c) 2019 IBM Corporation
SPDX-License-Identifier: LGPL-3.0-or-later

CONSTANTFOLD is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License as published by the Free Software
Foundation,either version 3 of the License, or (at your option) any
later version.

CONSTANTFOLD is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
CONSTANTFOLD.  If not, see <http://www.gnu.org/licenses/>.

|#

(in-package :cl-user)
(defpackage constantfold.test-asd
  (:use :cl :asdf))
(in-package :constantfold.test-asd)


(defsystem constantfold.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of constantfold"
  :license "LGPL"
  :depends-on (:constantfold
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (test-op :after (op c) (eval (read-from-string "(5am:run! :constantfold)"))
))
