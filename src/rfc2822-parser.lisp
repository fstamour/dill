
(in-package #:dill)

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
