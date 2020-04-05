
(defpackage #:dill.samples
  (:use #:cl #:alexandria)
  (:export
   #:find-sample-pathname))

(in-package #:dill.samples)

(defun find-component (keys &optional (system
				       (asdf:find-system "dill")))
  "Recusively search for an asdf component by name."
  (check-type keys list)
  (check-type system asdf/system:system)
  (let ((result (asdf:module-components system)))
    (loop
       :for key :in keys
       :for component = (find key
			      result
			      :test #'string=
			      :key #'asdf:component-name)
       :while component
       :do
	 (setf result
	       (if (typep component 'asdf/component:module)
		   (asdf:module-components component)
		   component)))
    (asdf:component-pathname result)))

(defun find-sample-pathname (sample-designator)
  "Find a sample by name using asdf"
  (etypecase sample-designator
      (string
       (find-component `("samples" ,sample-designator)))
      (keyword
       (find-sample-pathname
	(format nil "~(~a~).tgz" sample-designator)))))

#+nil
(list
  (find-sample-pathname :empty)
  (find-sample-pathname :empty-with-staged-readme)
  (find-sample-pathname :empty-readme))
