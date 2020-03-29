;;;; scratch code, repl ftw

(in-package #.dill.asd:project-name)

(defvar *dummy-repositories*
  "./dummy-git-repositories/")

(defvar *empty-repository*
  (merge-pathnames
   "dummy-repo-1/" 
   *dummy-repositories*))

(defvar *repository-with-a-readme*
  (merge-pathnames
   "dummy-repo-2/" 
   *dummy-repositories*))


#+nil
(let ((arguments
       (apply-argv:parse-argv
	(split-sequence:split-sequence
	 #\space
	 "-q --quiet --bare --template=somedir --separate-git-dir --shared=permissions dir"))))
  (if (listp (first arguments))
      `(,(caar arguments) ,@(rest arguments))
      arguments))


(defvar *first-commit*
  (let* ((repo (make-git-repository *repository-with-a-readme*))
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
	 (objpath (repo-obj-path repo hash)))
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

