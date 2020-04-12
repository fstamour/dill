
(defpackage #:dill
  (:documentation "Write your own git")
  (:nicknames :git)
  (:use :cl
	      #:alexandria
	      #:dill.utils)
  (:export))

(in-package #:dill)
