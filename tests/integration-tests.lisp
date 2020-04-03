
#+nil
(in-package #:dill.test)

(in-package #.dill.asd:project-name)

(defmacro with-temporary-directory ((var) &body body)
  `(let ((,var (cl-fad:pathname-as-directory
		(remove-last-newline
		 (with-output-to-string (*standard-output*)
		   (uiop:run-program '("mktemp" "-d") :output t))))))
     (unwind-protect (progn ,@body)
       (uiop:run-program (list "rm" "-r" (namestring ,var))))))

(defun test-init ()
  (with-temporary-directory (root)
    (command-init (list root))
    (multiple-value-bind (_ extra-paths missing-paths)
        (compare-hash-tables
         (list-tree-as-hash-table root)
         (list-hash-table
	        '(".git/HEAD"
	          ".git/"
	          ".git/branches/"
	          ".git/config"
	          ".git/description"
	          ".git/hooks/"
	          ".git/info/"
	          ".git/info/exclude"
	          ".git/objects/"
	          ".git/objects/info/"
	          ".git/objects/packs/"
	          ".git/refs/"
	          ".git/refs/heads/"
	          ".git/refs/tags/")
	        :test 'equal))
      (unless (emptyp extra-paths)
        (error "init create more stuff than expected:~%~{~& * \"~a\"~}"
	             extra-paths))
      (unless (emptyp missing-paths)
        (error "init failed to create some stuff :~%~{~& * \"~a\"~}"
	             missing-paths))))
  :passed)

