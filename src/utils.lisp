
(in-package #.dill.asd:project-name)

(defun length=1 (list)
  (null (cdr list)))


(defun parent-directory (pathname)
  "Create a pathname, it probably has bugs."
  (let ((parent (butlast (pathname-directory pathname))))
    (when parent
      (make-pathname :directory parent))))


(defun split-by-newline (string)
  "Split a string by newlines, treat multiple newlines as one."
  (split-sequence:split-sequence
   #\Newline string
   :remove-empty-subseqs t))


;; The equivalent regex:
;; "\\\\\\s*\\n"
(defvar +join-line-on-backspace-regex+
  (cl-ppcre:create-scanner
   '(:sequence #\\
     (:greedy-repetition 0 nil :whitespace-char-class)
     #\newline))
  "The regex used in the function join-line-on-backspace.")

(defun join-line-on-backspace (string)
  (cl-ppcre:regex-replace-all +join-line-on-backspace-regex+ string ""))

#+nil
(progn
  (cl-ppcre:regex-replace-all +join-line-on-backspace-regex+ +default-git-configuration+ "")
  (cl-ppcre:regex-replace-all +join-line-on-backspace-regex+ "

he\\  
llo

" ""))

(defun remove-last-newline (string)
  (subseq string 0
	  (if (alexandria:ends-with #\newline string)
	      (1- (length string))
	      nil)))

#+nil
(equal
 (remove-last-newline "asdf")
 (remove-last-newline "asdf
"))


;; inneficient, but it works
;; see cl-fad:merge-pathnames-as-file for faster, but not as user-friendly
(defun join-path (root &rest parts)
  (let ((path root)
	(last-part (alexandria:lastcar parts)))
    (loop :for part :in (butlast parts)
       :do (setf path (merge-pathnames (format nil "~a/" part) path)))
    (merge-pathnames last-part path)))

(defun list-hash-table (list &rest make-hash-table-arguments)
  "Convert a list to a hash-table where the keys are the elements of the
list and the values are T."
  (let ((hash-table (apply #'make-hash-table make-hash-table-arguments)))
    (loop :for element :in list
       :do (setf (gethash element hash-table) t))
    hash-table))

(defun hash-table-key-union (ht1 ht2)
  "Compute a list of keys that are present in both hash-tables"
  (uiop:while-collecting (collect-union)
    (loop :for key :being :the :hash-key :of ht1
       :when(gethash key ht2)
       :do (collect-union key))))

(defun hash-table-key-difference (ht1 ht2)
  "Compute a list of keys that are present only in ht1"
  (uiop:while-collecting (collect-difference)
    (loop :for key :being :the :hash-key :of ht1
       :unless (gethash key ht2)
       :do (collect-difference key))))

(defun compare-hash-tables (ht1 ht2)
  "Returns 3 values:
1. The list of keys common to both hash-table.
2. The list of keys present in ht1 only
3. The list of keys present in ht2 only "
  (values
   (hash-table-key-union ht1 ht2)
   (hash-table-key-difference ht1 ht2)
   (hash-table-key-difference ht2 ht1)))

#+nil
(compare-hash-tables
 (list-hash-table '('a 'b 'a 'd) :test 'equal)
 (list-hash-table '('c 'b 'a) :test 'equal))
;; => ('A 'B), ('D), ('C)


(defun list-tree (root)
  "List all the files and folders in a path, it returns a hash-table
with the paths as keys and T as values (basically a set)."
  (let ((files (make-hash-table :test 'equal)))
    (cl-fad:walk-directory
     root
     #'(lambda (pathname
		&aux (relative-path (enough-namestring pathname root)))
	 (when (not (zerop (length relative-path)))
	   (setf (gethash relative-path files) t)))
     :directories t)
    files))
