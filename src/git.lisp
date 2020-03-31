;;;; This is where I put the code that is worth putting in the image,
;;;; but that doesn't yet belong to a specific category.

(in-package #.dill.asd:project-name)

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

(defun find-object (object-specifier)
  (cond
    ((sha1p object-specifier) object-specifier)
    ((hexp object-specifier)
     )))


(defun parse-object-header (object)
  "extract an object's header"
    (let ((parts
	   (split-sequence:split-sequence
	    #\space
	    (flexi-streams:octets-to-string
	     (subseq object
		     0
		     (position 0 object))))))
      (list (first parts) (parse-integer (second parts)))))

(defun get-object-data (object)
  "extract an object's data"
  (flexi-streams:octets-to-string
   (subseq object (1+ (position 0 object)))))

(defun parse-author (string)
  (multiple-value-bind
	(start end register-starts register-ends)
      (cl-ppcre:scan "^([^<]+) <([^>]+)> (\\d+) (.+)$" string)
    (declare (ignore start end))
    (loop :for start :across register-starts
       :for end :across register-ends
       :collect (subseq string start end))))


