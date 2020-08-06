
(in-package #:dill.test)

(defun sort-alist (alist)
  "Sort an alist alphabetically, by #'car."
  (sort alist #'string< :key #'car))

(defun config-alist (config)
  "Create a nested alist from a nested hash-table. The keys are sorted
to be more deterministic."
  (mapcar #'(lambda (cons)
	      (cons (car cons)
		    (sort-alist
		     (hash-table-alist (cdr cons)))))
	  (sort-alist
	   (hash-table-alist config))))



(define-test parse-config
  (is equal
      '(("core"
	 ("bare" . :FALSE)
	 ("filemode" . :TRUE)
	 ("logallrefupdates" . :TRUE)
	 ("repositoryformatversion" . 0)))
      (config-alist
       (dill::parse-config "

# a comment
[core]
    repositoryformatversion = 0
    filemode = true # another comment
    bare = false
    logallrefupdates = true
    # and multiline comments \\
 that goes on and on


"))))


(define-test parse-ref
  (is string=
      "refs/heads/master"
      (dill::parse-ref "ref: refs/heads/master")))
