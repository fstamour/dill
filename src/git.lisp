;;;; This is where I put the code that is worth putting in the image,
;;;; but that doesn't yet belong to a specific category.

(in-package #:dill)

(defun repo-object-path (repository &optional hash)
  "Compute the path of an object given its hash"
  (if hash
      (join-path (gitdir repository)
		 "objects/"
		 (subseq hash 0 2)
		 (subseq hash 2))
      (join-path (gitdir repository) "objects/")))

(defun repo-list-object (repository)
  "List all objects in the repository"
  (uiop:while-collecting (collect-file)
    (cl-fad:walk-directory
     (repo-object-path repository)
     #'(lambda (pathname)
	 (collect-file
	  (concatenate
	   'string
	   (alexandria:lastcar (pathname-directory pathname))
	   (pathname-name pathname))))
     :test #'(lambda (pathname)
	       (and
		(pathname-name pathname)
		(let ((parent-directory-name
		       (alexandria:lastcar (pathname-directory pathname))))
		  (and (= 2 (length parent-directory-name))
		       (hexp parent-directory-name)))))
     :directories t)))

(defun repo-find-object (repo object-specifier)
  "Find an object given a specifier (a hash, a partial hash)"
  (cond
    ;; If it's a full hash
    ((sha1p object-specifier) object-specifier)
    ;; If it's a partial hash
    ((hexp object-specifier)
     (let ((candidates
             (remove-if-not
              (alexandria:curry #'alexandria:starts-with-subseq object-specifier)
              (repo-list-object repo))))
       (if (length=1 candidates)
           (first candidates)
           ;; else: TODO use a restart
           )))))

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

(defun parse-object-header (object)
  "extract an object's header"
    (let ((parts
	   (split-sequence:split-sequence
	    #\space
	    (flexi-streams:octets-to-string
	     (subseq object
		     0
		     (position 0 object))))))
      (values (first parts) (parse-integer (second parts)))))

(defun get-object-data (object)
  "extract an object's data"
  (subseq object (1+ (position 0 object))))

(defun get-object-data-as-string (object)
  "extract an object's data and convert it to a string"
  (flexi-streams:octets-to-string
   (get-object-data object)))

(defun parse-object (object)
  (values
   (parse-object-header object)
   (get-object-data object)))

(defun parse-author (string)
  (multiple-value-bind
	(start end register-starts register-ends)
      (cl-ppcre:scan "^([^<]+) <([^>]+)> (\\d+) (.+)$" string)
    (declare (ignore start end))
    (loop :for start :across register-starts
       :for end :across register-ends
       :collect (subseq string start end))))

(defun ensure-octets (data)
  (typecase data
    (string (flexi-streams:string-to-octets data))
    (flexi-streams:octet data)
    (t data)))

(defun make-object (type data)
  (let* ((length (length data))
	 (header (format nil "~(~a~) ~d~c" type length #\Null)))
    (flexi-streams:with-output-to-sequence
	(stream :element-type 'flexi-streams:octet)
      (write-sequence (flexi-streams:string-to-octets header) stream)
      (write-sequence (ensure-octets data) stream))))
