
(in-package #.dill.asd:project-name)

(defparameter +commands+
  '(("add" . command-add)
    ("cat-file" . command-cat-file)
    ("checkout" . command-checkout)
    ("commit" . command-commit)
    ("hash-object" . command-hash-object)
    ("help" . command-help)
    ("init" . command-init)
    ("log" . command-log)
    ("ls-tree" . command-ls-tree)
    ("merge" . command-merge)
    ("rebase" . command-rebase)
    ("rev-parse" . command-rev-parse)
    ("rm" . command-rm)
    ("show-ref" . command-show-ref)
    ("status" . command-status)
    ("tag" . command-tag)))



(defun command-init ()
  (error "Not implemented"))


(defun command-add ()
  (error "Not implemented"))

(defun command-cat-file ()
  (error "Not implemented"))

(defun command-checkout ()
  (error "Not implemented"))

(defun command-commit ()
  (error "Not implemented"))

(defun command-hash-object ()
  (error "Not implemented"))

(defun command-help ()
  (let* ((prelude (format nil "usage: ~a " +project-name+))
	 (prelude-length (length prelude)))
    (format t (concatenate 'string
			   "~&~a[--help]~%"
			   "~v@{~A~:*~}"
			   "<command> [<args>]~%~%"
			   "~AAvailable commands: ~{~(~a~)~}")
	    prelude
	    prelude-length " "
	    +commands+)))

(defun command-log ()
  (error "Not implemented"))

(defun command-ls-tree ()
  (error "Not implemented"))

(defun command-merge ()
  (error "Not implemented"))

(defun command-rebase ()
  (error "Not implemented"))

(defun command-rev-parse ()
  (error "Not implemented"))

(defun command-rm ()
  (error "Not implemented"))

(defun command-status ()
  (error "Not implemented"))

(defun command-show-ref ()
  (error "Not implemented"))

(defun command-tag ()
  (error "Not implemented"))

(defun find-command (name)
  (cdr (assoc name +commands+ :test 'equal)))

(defun suggest-command (input)
  (let ((best-candidate nil)
	(best-score 0.0))
    (loop
       :for (name . _) :in +commands+
       :for score = (vas-string-metrics:jaro-winkler-distance input name)
       :when (or
	      (> score best-score)
	      (null best-candidate))
       :do (setf best-candidate name
		 best-score score))
    (when (> best-score 0.80)
      (format t "~&~%The most similar command is~%        ~a"
	      best-candidate)
      best-candidate)))

#+nil (suggest-command "statsu")

(defun unknown-command (command-name)
  (format t "~&~a: '~a' is not a ~0@*~a command. See '~0@*~a --help'"
	  +project-name+ command-name)
  (suggest-command command-name))


(defun main (args)
  (let* ((first-argument (second args))
	 (command (find-command first-argument))
	 (command-arguments (cddr args)))
    (cond
      ((null first-argument)
       (command-help))
      ((null command)
       (unknown-command first-argument))
      (t
       (apply command command-arguments)))))

(defparameter uiop/image:*image-entry-point*
  #'(lambda ()
      (main (uiop/image:raw-command-line-arguments))))
