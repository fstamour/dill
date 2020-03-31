;;;; scratch code, repl ftw

(in-package #.dill.asd:project-name)

(defvar *dummy-repositories*
  "./dummy-git-repositories/")

(defparameter *empty-repository*
  (make-git-repository
   (merge-pathnames
    "dummy-repo-1/" 
    *dummy-repositories*)))

(defparameter *repository-with-a-readme*
  (make-git-repository
   (merge-pathnames
    "dummy-repo-2/" 
    *dummy-repositories*)))

(defun repo-read-head (repository)
  "Read the HEAD of a repository"
  (parse-ref
   (remove-last-newline
    (alexandria:read-file-into-string
     (merge-pathnames "HEAD" (gitdir repository))))))

(defun repo-resolve-ref (repository ref)
  "Resolve a ref"
  (remove-last-newline
   (alexandria:read-file-into-string
    (merge-pathnames ref (gitdir repository)))))


(let* ((repo *repository-with-a-readme*)
       (ref (repo-read-head repo))
       (hash (repo-resolve-ref repo ref)))
  (sha1p hash))


(repo-list-object *repository-with-a-readme*)


;; RENDU: given a list and a string
;; find an item that start by the string
;; and make sure it's the only one
;; could try to "find-all"
(defun find-)


(position "08f"
	  '(
	    "c29d55b1dd717f9942c1dc9b9c8e201dcb1bcaa8"
	    "08f72"
	    "08f4360732d08448be6eeccc8a9036fa432e1bbe"
	    "ea6b1222f0a57b059054a29dfcd48eaccdbb2fc7")
	  :test #'alexandria:starts-with-subseq
	  )

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



