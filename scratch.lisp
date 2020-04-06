;;;; scratch code, repl ftw

(in-package #:dill)

(defparameter *empty-readme-vfs*
  (make-instance 'memory-backed-vfs
		 :content
		 (vfs:read-archive-into-memory
		  (dill.samples:find-sample-pathname :empty-readme))))

(uiop:while-collecting (collect)
  (vfs:vfs-map *empty-readme-vfs*
	       #'collect))

(uiop:while-collecting (collect)
  (vfs:vfs-map-files *empty-readme-vfs*
	       #'collect))

(uiop:while-collecting (collect)
  (vfs:vfs-map-directories *empty-readme-vfs*
	       #'collect))

(maphash #'(lambda (key value)
	     (when (eq :directory (first value))
	       (print key)))
	 (slot-value *empty-readme-vfs* 'vfs::content))

(defparameter *in-memory-repository*
  (make-git-repository
   ))

(let* ((repo *repository-with-a-readme*)
       (ref (repo-read-head repo))
       (hash (repo-resolve-ref repo ref)))
  (sha1p hash))

(repo-list-object *repository-with-a-readme*)
("08f4360732d08448be6eeccc8a9036fa432e1bbe"
 "c29d55b1dd717f9942c1dc9b9c8e201dcb1bcaa8"
 "ea6b1222f0a57b059054a29dfcd48eaccdbb2fc7")

(mapcar
 (alexandria:curry
  #'repo-find-object *repository-with-a-readme*)
 '("08f72"
   "08f4"))
;; => (NIL "08f4360732d08448be6eeccc8a9036fa432e1bbe")


(defvar *first-commit*
  (let* ((repo *repository-with-a-readme*)
	 ;; read HEAD
	 (ref (parse-ref
	       (remove-last-newline
		(alexandria:read-file-into-string
		 (merge-pathnames "HEAD" (gitdir repo))))))
	 ;; read the ref
	 (hash (remove-last-newline
		(alexandria:read-file-into-string
		 (merge-pathnames ref (gitdir repo)))))
	 ;; compute the path to the object named "hash"
	 (objpath (repo-object-path repo hash)))
    (zlib:uncompress (alexandria:read-file-into-byte-vector objpath))))


;; (parse-object-header *first-commit*)
;; => ("commit" 208)
#|
parse-object-header returns a list of 2 items:
the type of object and the length of the object
the type can be one of these:
 * commit
 * tree
 * tag
 * blob
|#

(defun hash-vector)

(ql:quickload :ironclad)

(defparameter *octets*
  (ironclad:digest-sequence :sha1
			    (flexi-streams:string-to-octets "hi")))

(with-output-to-string (stream)
 (loop :for octets :across *octets*
      :do (format stream "~(~x~)" octets)))

(map 'string #'code-char *octets*)





(let ((object (make-object :blob "hello")))
  (parse-object object))
