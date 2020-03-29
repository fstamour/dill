
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


(defvar +default-git-configuration+
    "

# a comment
[core]
    repositoryformatversion = 0
    filemode = true # another comment  
    bare = false
    logallrefupdates = true
    # and multiline comments \\
 that goes on and on


"
  "Just a sample config to test the parser")


(defun lex-line (string make-lexer-fn)
  "Tokenize a string
Take a string and a lexer-constructor."
  (loop :with lexer := (funcall make-lexer-fn string)
        :for tok := (funcall lexer)
        :while tok
          :collect tok))


(alexa:define-string-lexer config-lexer
    ()
    ;; ignore whitespaces
    ("\\s+" nil)
    ;; comments
    ("#.*" nil)
    ;; equal
    ("=" (return :=))
    ;; integers
    ("[\\d]+" (return (parse-integer $@)))
    ;; booleans
    ("true" (return :true))
    ("false" (return :false))
    ;; section name
    ("\\[(?<NAME>[^\\]]+)\\]" (return $NAME))
    ;; anything else is as-is
    ("[^\\s]+" (return $@)))

(defun parse-config (string
		     &optional (config
				(make-hash-table :test 'equal)))
  "Parse a config file, returns a recursive hash-table.
The top-level hash-table is the sections, the other is the key-values"
  (let ((current-section))
    (loop :for line :in (split-by-newline
			 (join-line-on-backspace string))
       :for form = (lex-line line #'config-lexer)
       :when form
       :collect
	 (optima:match
	  form
	  ;; Section name
	  ((list section-name)
	   (setf current-section section-name
		 (gethash current-section config)
		 (make-hash-table :test 'equal)))
	  ;; Key = Value
	  ((list* key := value-list)
	   (setf (gethash key
			  (gethash current-section config))
		 (if (length=1 value-list)
		     (first value-list)
		     value-list)))))
    config))

#+nil
(let ((config
       (parse-config
	+default-git-configuration+)))
  (mapcar #'(lambda (el)
	      (if (hash-table-p el)
		  (hash-table-plist el)
		  el))
	  (hash-table-plist config)))

;; TODO
(defun make-git-bare-repository (gitdir
				 &optional
				   (worktree (parent-directory gitdir))))

(defun get-config (git-repository section key &optional default)
  (or (access:accesses (configuration git-repository) section key)
      default))

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

#+nil
(get-config 
 (make-git-repository *empty-repository*)
 "core"
 "repositoryformatversion")
;; => 0

#+nil
(get-config 
 (make-git-repository *empty-repository*)
 "unicorn"
 "don't exists")
;; => nil


(defun parse-ref (string)
  (second
   (split-sequence:split-sequence
    #\space string
    :remove-empty-subseqs t)))


(defun repo-obj-path (repository hash)
  (join-path (gitdir repository)
	     "objects"
	     (subseq hash 0 2)
	     (subseq hash 2)))

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

(defun parse-object-header (object)
    (let ((parts
	   (split-sequence:split-sequence
	    #\space
	    (flexi-streams:octets-to-string
	     (subseq object
		     0
		     (position 0 object))))))
      (list (first parts) (parse-integer (second parts)))))

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

(defun get-object-data (object)
  (flexi-streams:octets-to-string
   (subseq object (1+ (position 0 object)))))

(defun position-double-newline (string)
  "Find the position of the first 2 consecutive newlines"
  (let ((last-newline (position #\newline string)))
    (loop :for newline = (position #\newline string
				   :start (1+ last-newline))
       :while (and newline
		   (not (eq 1 (- newline last-newline))))
       :do (setf last-newline newline))
    last-newline))


#+nil
(let ((string  "012
45
 89
a
 b"))
  (flet ((clean (str)
	   (cl-ppcre:regex-replace-all "\\n " str "")))
    (let ((last 0))
      (append
       (loop :for (start end) :on (cl-ppcre:all-matches "\\n[^ ]" string)
	  :by #'cddr
	  :collect (clean (subseq string last start))
	  :do (setf last (1- end)))
       (list (clean (subseq string last)))))))

(defun extract-rfc2822-body (string)
  (remove-last-newline
   (subseq string (+ 2 (position-double-newline string)))))

;; (extract-rfc2822-body (get-object-data *first-commit*))
;; => "add a very interesting readme"

(defun parse-rfc2822-header (string)
    (flet ((clean (str)
	     ;; remove "\\n "
	     (cl-ppcre:regex-replace-all "\\n " str ""))
	   (split (str)
	     (let ((position (position #\space str)))
	       (list (subseq str 0 position)
		     (subseq str (1+ position))))))
      (let ((last 0))
	(append
	 (loop :for (start end) :on (cl-ppcre:all-matches "\\n[^ ]" string)
	    :by #'cddr
	    :collect (split (clean (subseq string last start)))
	    :do (setf last (1- end)))
	 (list (split (clean (subseq string last))))))))


#|
(parse-rfc2822-header (extract-rfc2822-header
		     (get-object-data *first-commit*)))
=>
(("tree" "c29d55b1dd717f9942c1dc9b9c8e201dcb1bcaa8")
 ("author" "Francis St-Amour <m_psyco2@hotmail.com> 1585274479 -0400")
 ("committer" "Francis St-Amour <m_psyco2@hotmail.com> 1585274479 -0400"))
|#

(defun parse-author (string)
  (multiple-value-bind
	(start end register-starts register-ends)
      (cl-ppcre:scan "^([^<]+) <([^>]+)> (\\d+) (.+)$" string)
    (declare (ignore start end))
    (loop :for start :across register-starts
       :for end :across register-ends
       :collect (subseq string start end))))


