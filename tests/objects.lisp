
(in-package #:dill.test)

(define-test todo-find-a-name
  (is equalp #(52 0 1 2 3 4)
      (git::make-object #(1 2 3 4)))
  (is equalp #(53 0 104 101 108 108 111)
      (git::make-object "hello")))

;; TODO make an object, read it, make it again
