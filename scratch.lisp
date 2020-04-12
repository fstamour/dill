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

(defparameter *in-memory-repository*
  (make-git-repository *empty-readme-vfs*))

(let* ((repo *repository-with-a-readme*)
       (ref (repo-read-head repo))
       (hash (repo-resolve-ref repo ref)))
  (sha1p hash))

(repo-list-object *repository-with-a-readme*)

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
