(defpackage mqsort
  (:use :common-lisp)
  (:shadow :common-lisp sort)
  (:export sort))
(in-package :mqsort)

(defvar *fastest* '(optimize (speed 3) (safety 0) (debug 0)))
(defvar *interface* '(optimize (speed 3) (safety 1) (debug 1)))

(deftype array-index () `(mod ,array-dimension-limit))
(deftype most-efficient-string () '(simple-array character))