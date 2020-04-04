
(in-package #:dill.test)

(uiop:setup-temporary-directory)

;; Run all tests and exit the process with non-zero code if any tests failed.
(unless
    (every #'(lambda (x)
               (eq :passed (parachute:status x)))
           (parachute:results
            (test-all)))
  (uiop:quit 1))

