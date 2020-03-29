
(in-package #.dill.asd:project-name)

(defclass git-repository ()
  ((worktree
    :initform nil
    :initarg :worktree
    :accessor worktree)
   (gitdir
    :initform (error "gitdir is required")
    :initarg :gitdir
    :accessor gitdir)
   (configuration
    :initform (error "configuration is required")
    :initarg :configuration
    :accessor configuration)))


;; TODO
(defun make-git-bare-repository (gitdir
				 &optional
				   (worktree (parent-directory gitdir))))

(defun make-git-repository (worktree &optional
				       (gitdir (merge-pathnames
						".git/" 
						worktree)))
  (make-instance 'git-repository
		 :worktree worktree
		 :gitdir gitdir
		 :configuration (parse-config
				 (alexandria:read-file-into-string 
				  (merge-pathnames "config" gitdir)))))

(defun get-config (git-repository section key &optional default)
  (or (access:accesses (configuration git-repository) section key)
      default))

(defun gitdir-join-path (repo &rest paths)
  "Compute a path under the repository's .git directory"
  (apply #'join-path (gitdir repo) paths))

#+nil
(gitdir-join-path
 (make-git-repository *repository-with-a-readme*)
 "objects/")

(defun worktree-join-path (repo &rest paths)
  "Compute a path under the repository's worktree directory"
  (apply #'join-path (worktree repo) paths))
