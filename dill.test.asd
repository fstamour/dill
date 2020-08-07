
(require '#:dill)

(in-package #:dill.asd)

(defsystem :dill.test
  :description "Tests for dill (Implementation of git purely in common lisp)"
  :version "0.0.1"
  :author "Francis St-Amour"
  :licence "GNU GPLv3"
  :depends-on (:dill :parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :dill.test :test-all))
  :serial t
  :components ((:module tests
			:components
			((:file "package")
			 (:file "config-parser")
			 (:file "objects")
			 (:file "commands")
			 (:file "integration-tests")
			 (:file "tests")))))

