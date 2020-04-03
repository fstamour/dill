
(defpackage #:dill.test
  (:use :cl #.dill.asd:project-name #:alexandria)
  (:import-from
   #:parachute
   #:define-test
   #:is #:isnt
   #:true #:false
   #:fail)
  (:export #:test-all))


