
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
      (git::compare-hash-tables
       (git::list-tree-as-hash-table root-pathname)
       (git::list-hash-table expected-pathnames :test 'equal))
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
     (git::command-init (list root))
     (check-expected-pathnames root +git-init-expected-pathnames+))))

