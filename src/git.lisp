;;;; This is where I put the code that is worth putting in the image,
;;;; but that doesn't yet belong to a specific category.

(in-package #.dill.asd:project-name)

(defun repo-obj-path (repository hash)
  "Compute the path of an object given its hash"
  (join-path (gitdir repository)
	     "objects"
	     (subseq hash 0 2)
	     (subseq hash 2)))

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


