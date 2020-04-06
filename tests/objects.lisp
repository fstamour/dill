
(in-package #:dill.test)

(define-test todo-find-a-name
  (is equalp
      #(98 108 111 98 32 52 0 1 2 3 4)
      (dill::make-object "blob" #(1 2 3 4)))
  (is equalp
      #(98 108 111 98 32 53 0 104 101 108 108 111)
      (dill::make-object :blob "hello")))

;; TODO make an object, read it, make it again
