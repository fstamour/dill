#|
Ideas:

* List the objects in the file system
  Then try to call "repo-find-object" on each.

|#

(in-package #:dill.test)

(define-test integration)

(defun random-identifier (length)
  (let ((result (make-string length)))
    (loop :for i :below length
          :do (setf (aref result i)
                    (alexandria:random-elt "abcdefghjkmnpqrstuvwxyz23456789")))
    result))

(defun make-temporary-directory ()
  (loop :for name = (random-identifier 10)
        :for pathname = (merge-pathnames
                         (format nil "tmp.~a/" name)
                         (uiop:temporary-directory))
        ;; Stop after a hundred time
        :for i :below 100
        :while (cl-fad:directory-exists-p pathname)
        :finally (progn
                   (print i)
                   (ensure-directories-exist pathname)
                   (return pathname))))

(defmacro with-temporary-directory ((var) &body body)
  `(let ((,var (make-temporary-directory)))
     (unwind-protect (progn ,@body)
       (uiop:delete-directory-tree ,var :validate t))))

(defun check-expected-pathnames (root-pathname expected-pathnames)
  (multiple-value-bind (_ extra-paths missing-paths)
      (dill::compare-hash-tables
       (dill::list-tree-as-hash-table root-pathname)
       (dill::list-hash-table expected-pathnames :test 'equal))
    (declare (ignore _))
    (unless (emptyp extra-paths)
      (error "There was more pathnames than expected:~%~{~& * \"~a\"~}"
	           extra-paths))
    (unless (emptyp missing-paths)
      (error "Some pathnames were missing :~%~{~& * \"~a\"~}"
	           missing-paths))))

(defvar +git-init-expected-pathnames+
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
	  ".git/refs/tags/"))

(define-test (integration "Tests git init in an empty directory") ()
  (false
   (with-temporary-directory (root)
     (dill::command-init (list root))
     (check-expected-pathnames root +git-init-expected-pathnames+))))

(define-test (integration "Open an empty git-repository") ()
  (true
   (with-temporary-directory (root)
     (dill::command-init (list root))
     (let ((repository
	    (dill::make-git-repository root)))
       (zerop
	  (dill::get-config repository "core" "repositoryformatversion"))))))

(define-test (integration "Open an empty git-repository") ()
  (true
   (with-temporary-directory (root)
     (dill::command-init (list root))
     (let ((repository
	           (dill::make-git-repository root)))
       (zerop
	      (dill::get-config repository "core" "repositoryformatversion"))))))
