
(defpackage #.dill.asd:project-name
  (:documentation "Write your own git")
  (:nicknames :git)
  (:use :cl #:alexandria))

(in-package #.dill.asd:project-name)

(defvar +project-name+ (string-downcase #.dill.asd:project-name))