
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

