
(defpackage #:dill
  (:documentation "Write your own git")
  (:nicknames :git)
  (:use :cl
	#:alexandria
	#:virtual-file-system
	#:dill.utils))

(in-package #:dill)
