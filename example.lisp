
(in-package :cl-user)
(defpackage :constantfold-example
  (:use :cl :trivia :alexandria :iterate
        :lisp-namespace
        :constantfold))

(in-package :constantfold-example)

(defstruct point2d
  (x 0 :type real)
  (y 0 :type real))

(defmethod make-load-form ((o point2d) &optional env)
  (make-load-form-saving-slots o :environment env))

(defun point2d-add/2 (p1 p2)
  (ematch* (p1 p2)
    (((point2d :x x1 :y y1)
      (point2d :x x2 :y y2))
     (make-point2d :x (+ x1 x2) :y (+ y1 y2)))))

(defun point2d-add (&rest args)
  (reduce #'point2d-add/2 args))

(print (point2d-add #S(point2d :x 1 :y 2) #S(point2d :x 1 :y 2) #S(point2d :x 1 :y 2)))

(constantfold make-point2d :copier copy-point2d)
(constantfold point2d-add  :copier copy-point2d :commutative t :associative t)

(defun fn1 ()
  (point2d-add (make-point2d :x 1 :y 2) (make-point2d :x 3 :y 4)))

(defun fn1 ()
  (point2d-add #S(point2d :x 1 :y 2) #S(point2d :x 3 :y 4)))

;; (copy-point2d #S(point2d :x 4 :y 6))

(print (fn1))
(print (fn1))
(assert (not (eq (fn1) (fn1))))

(defun fn2 ()
  (point2d-add #S(point2d :x 1 :y 2) #S(point2d :x 3 :y 4)))

;; #S(point2d :x 4 :y 6)

(print (fn2))
(print (fn2))
(assert (not (eq (fn2) (fn2))))

(defun fn3 (p1 p2 x y)
  (point2d-add (make-point2d :x 1 :y 2)
               p1
               (make-point2d :x 3 :y 4)
               p2
               (make-point2d :x x :y y)))

(print (fn3 (make-point2d :x 3 :y 4) (make-point2d :x 3 :y 4) 10 10))

#+(or)
(point2d-add (copy-point2d #S(point2d :x 4 :y 6))
             p1
             p2
             (make-point2d :x x :y y))


(defun fn4 (x) (point2d-add (make-point2d :x x :y 2)
                            (make-point2d :x 3 :y 4)))

(print (fn4 10))





(defun fn5 (x y)
  (point2d-add (make-point2d :x 1 :y 1)
               (point2d-add (make-point2d :x 1 :y 1)
                            (make-point2d :x x :y y)
                            (make-point2d :x 1 :y 1))
               (make-point2d :x 1 :y 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; immutable

(defstruct point2dm
  (x 0 :type real)
  (y 0 :type real))

(defmethod make-load-form ((o point2dm) &optional env)
  (make-load-form-saving-slots o :environment env))

(defun point2dm-add/2 (p1 p2)
  (ematch* (p1 p2)
    (((point2dm :x x1 :y y1)
      (point2dm :x x2 :y y2))
     (make-point2dm :x (+ x1 x2) :y (+ y1 y2)))))

(defun point2dm-add (&rest args)
  (reduce #'point2dm-add/2 args))

(constantfold make-point2dm)
(constantfold point2dm-add :commutative t :associative t)

(defun fn1 ()
  (point2dm-add (make-point2dm :x 1 :y 2) (make-point2dm :x 3 :y 4)))

(defun fn5 (x y)
  (point2dm-add (make-point2dm :x 1 :y 1)
                (point2dm-add (make-point2dm :x 1 :y 1)
                              (make-point2dm :x x :y y)
                              (make-point2dm :x 1 :y 1))
                (make-point2dm :x 1 :y 1)))
