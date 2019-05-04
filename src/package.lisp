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
(defpackage :constantfold
  (:use :cl :trivia :alexandria :iterate
        :lisp-namespace)
  (:export :constantfold
           :unfold))
(in-package :constantfold)

;; blah blah blah.

(defpattern whole-form (name args)
  "Matches against the &whole argument of a compiler macro."
  `(or (list* 'funcall (list 'function ,name) ,args)
       (list* ,name ,args)))

(defun compiler-macro-form-name (whole)
  "Extracts the name of the function from the &whole argument of a compiler macro."
  (match whole
    ((whole-form name _) name)))


#|

(arange 5) should compile to (copy-array #(0 1 2 3 4))

(+ (arange 5) (arange 1 6)) should compile to (copy-array #(1 3 5 7 9))

(+ (arange 5) (arange 1 6))
==
(+ (copy-array #(0 1 2 3 4))
   (copy-array #(1 2 3 4 5)))
==
(copy-array #(1 3 5 7 9))

hmm

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; user defined constant form

(define-namespace constant-form
    symbol nil
    "The namespace for user-defined constant forms. The value holds the copier name.
For a symbol X, a form (X ...) is considered as a constant form.
When the copier is NIL, the value (X ...) is evaluated in compile-time.

When the copier is non-NIL, the copier itself is also registered as a constant
form with the same copier.
")

(defun register-constant-form (name &optional copier)
  (setf (symbol-constant-form name) copier)
  (when copier
    (setf (symbol-constant-form copier) copier)))


(defun constant-form-p (form &optional env)
  "Recursively checks if the form is a constant form.
Returns a boolean.  When FORM is a standard constant form reconized by
constantp, it also returns T as the secondary value."
  (or (values (constantp form env) t)
      (match form
        ;; if not list, this returns nil
        ((whole-form name args)
         (and (constant-form-boundp name)
              (every (rcurry #'constant-form-p env) args))))))

(defun constant-form-copier (name)
  (symbol-constant-form name))

(defun wrap-notinline (name form)
  "Suppresses further calls to the compiler macro which otherwise causes an infinite loop."
  `(locally
       (declare (notinline ,name))
     ,form))

(defun wrap-inline (name form)
  "Allows further calls to the compiler macro."
  `(locally
       (declare (inline ,name))
     ,form))

(defun evaluate-constants (name args)
  (match (constant-form-copier name)
    (nil
     ;; immutable
     ;; or CL constants
     ;; e.g. #(0 0), '(0 0), #S(struct ...), "..."

     ;; Note that modifying these constants result in an
     ;; unintuitive behavior. We interpret that the
     ;; results of constant-folded functions share the
     ;; same charactersitics. That is, given a function FN
     ;; with a constant folding compiler macro, the result
     ;; of calling FN with read-time constants may result
     ;; in a compile-time constant that is unsafe
     ;; to modify.
     `(quote ,(eval `(,name ,@args))))
    (copier
     ;; if the copiers are non-nil -- mutable, needs copying.
     (wrap-notinline copier
                     `(,copier (quote ,(eval `(,name ,@args))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constant folding

(defun flatten-associative-nested-fn (form)
  "Flattens a tree of forms of the same functions.
For example, (+ (+ 2 3) (+ 5 6)) into (+ 2 3 5 6)."
  (let ((name (compiler-macro-form-name form))
        (acc nil))
    (labels ((rec (form)
               (match form
                 ((whole-form (eq name) args)
                  (iter (for arg in args)
                        (rec arg)))
                 (_
                  (push form acc)))))
      (rec form)
      `(,name ,@(nreverse acc)))))

;; (flatten-associative-nested-fn '(+ (+ 2 3) (+ 5 6)))
;; (+ 2 3 5 6)

(define-compiler-macro foldable-commutative-associative-fn (&whole whole &rest args &environment env)
  "Performs constant-folding for commutative and associative functions.
Following optimization is performed:

  (+ a 2 3 b 1) --> (+ (+ 2 3 1) a b) --> (+ 6 a b)"
  (declare (ignorable args))
  (%foldable-commutative-associative-fn whole env))

(defun %foldable-commutative-associative-fn (whole env)
  (let* ((whole2 (flatten-associative-nested-fn whole))
         (args  (cdr whole2))
         (name  (compiler-macro-form-name whole2))
         (constants  (remove-if-not (rcurry #'constant-form-p env) args))
         (non-consts (remove-if     (rcurry #'constant-form-p env) args)))
    (match* (constants non-consts)
      ((nil _)
       whole)
      ((_ nil)
       (values (evaluate-constants name constants) t))
      ((_ _)
       (values (wrap-notinline name
                               `(,name ,(evaluate-constants name constants)
                                       ;; while non-consts are not constants,
                                       ;; its subforms may contain constants
                                       ,@(mapcar (curry #'wrap-inline name)
                                                 non-consts)))
               t)))))

(define-compiler-macro foldable-associative-fn (&whole whole &rest args &environment env)
  "Performs constant-folding for associative functions.
Following optimization is performed:

  (+ a 2 3 b) --> (+ a (+ 2 3) b) --> (+ a 5 b)"
  (declare (ignorable args))
  (%foldable-associative-fn whole env))

(defun %foldable-associative-fn (whole env)
  (let* ((whole2 (flatten-associative-nested-fn whole))
         (args  (cdr whole2))
         (name  (compiler-macro-form-name whole2)))
    (labels ((rec (rest tmp-consts args)
               (match* (rest tmp-consts)
                 ((nil nil)
                  (nreverse args))
                 ((nil _)
                  (nreverse (cons (evaluate-constants name (nreverse tmp-consts))
                                  args)))
                 (((list* now rest) nil)
                  (if (constant-form-p now env)
                      (rec rest (cons now tmp-consts) args)
                      (rec rest nil (list* (wrap-inline name now)
                                           args))))
                 (((list* now rest) _)
                  (if (constant-form-p now env)
                      (rec rest (cons now tmp-consts) args)
                      (rec rest nil (list* (wrap-inline name now)
                                           (evaluate-constants name (nreverse tmp-consts))
                                           args)))))))
      (if (some (rcurry #'constant-form-p env) args)
          (values (wrap-notinline name (rec args nil nil)) t)
          whole))))



(define-compiler-macro foldable-fn (&whole whole &rest args &environment env)
  "Performs a simple constant-folding.
Constantfold when all arguments are constants."
  (%foldable-fn whole args env))

(defun %foldable-fn (whole args env)
  (let ((name (compiler-macro-form-name whole)))
    (if (every (rcurry #'constant-form-p env) args)
        (values (evaluate-constants name args) t)
        whole)))

(defmacro constantfold (name &key commutative associative copier)
  "Registers a constant folding compiler macros to the function NAME."
  (cond
    ((and commutative associative)
     `(eval-when (:compile-toplevel :load-toplevel :execute)
        (register-constant-form ',name ',copier)
        (setf (compiler-macro-function ',name)
              (compiler-macro-function 'foldable-commutative-associative-fn))))
    (associative
     `(eval-when (:compile-toplevel :load-toplevel :execute)
        (register-constant-form ',name ',copier)
        (setf (compiler-macro-function ',name)
              (compiler-macro-function 'foldable-associative-fn))))
    (t
     `(eval-when (:compile-toplevel :load-toplevel :execute)
        (register-constant-form ',name ',copier)
        (setf (compiler-macro-function ',name)
              (compiler-macro-function 'foldable-fn))))))

(defun unfold (name)
  "Unregister the compiler macro"
  (setf (compiler-macro-function name) nil))
