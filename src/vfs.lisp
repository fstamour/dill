
(defpackage #:virtual-file-system
  (:nicknames #:vfs)
  (:use #:cl #:alexandria)
  (:export
   ;; classes
   #:vfs
   #:physical-vfs
   #:memory-backed-vfs

   ;; cl-like interface
   #:vfs-open
   #:vfs-close
   #:with-vfs-open-file

   ;; Support function
   #:read-archive-into-memory
   ))

(in-package #:virtual-file-system)

(defclass vfs ()
  ()
  (:documentation "Virtual file system"))

(defclass physical-vfs ()
  ()
  (:documentation "It's just the real file system"))

(defclass vfs-path ()
  ((vfs
     :initform (make-instance 'vfs)
     :initarg :vfs
     :accessor vfs-path-vfs
     :documentation "The VFS associated")
   (pathname
    :initform nil
    :initarg :pathname
    :accessor vfs-path-pathname))
  (:documentation "A pathname and its associated VFS."))

(defclass memory-backed-vfs (vfs)
  ((content
    :initform (make-hash-table :test 'equal)
    :initarg :content)))

(defgeneric vfs-open (vfs filespec &key direction element-type if-exists if-does-not-exist external-format)
  (:documentation "Open a file stream in a virtual file system"))

(defmethod vfs-open ((vfs physical-vfs) filespec
		     &rest options
		     &key direction element-type
		       if-exists if-does-not-exist
		       external-format)
  (declare (ignore vfs direction element-type
		       if-exists if-does-not-exist
		       external-format))
  (apply #'open filespec options))

(defgeneric vfs-close (vfs stream &key abort)
  (:documentation "Close a stream in a virtual file system"))

(defmethod vfs-close ((vfs physical-vfs) stream &key abort)
  (declare (ignore vfs))
  (close stream :abort abort))

(defmacro with-vfs-open-file (vfs (stream filespec &rest options
					  &key direction element-type
					  if-exists if-does-not-exist
					  external-format)
			      &body body)
  (declare (ignore direction element-type
		   if-exists if-does-not-exist
		   external-format))
  (check-type stream symbol)
  (multiple-value-bind (forms declarations documentation)
      (alexandria:parse-body body)
    (declare (ignore documentation))
    (let ((abortp (gensym)))
      (alexandria:once-only
	  (vfs)
	`(let ((,stream (vfs-open ,vfs ,filespec ,@options))
	       (,abortp t))
	   ,@declarations
	   (unwind-protect
		(multiple-value-prog1
		    (progn ,@forms)
		  (setq ,abortp nil))
	     (when ,stream
	       (vfs-close ,vfs ,stream :abort ,abortp))))))))

#+nil
(with-vfs-open-file (make-instance 'physical-vfs)
    (stream "./readme.md" :direction :input)
  (alexandria:read-stream-content-into-string stream))

(defun read-archive-into-memory (pathname &optional in-memory-archive)
  "Returns a \"flat\" hash-table"
  (when in-memory-archive
    (check-type in-memory-archive hash-table))
  (let ((result (or in-memory-archive
		    (make-hash-table :test 'equal))))
    (gzip-stream:with-open-gzip-file (stream pathname)
      (archive:with-open-archive (archive stream :direction :input)
	(archive:do-archive-entries (entry archive)
	  (let ((name (remove-prefix "./" (archive:name entry)))
		(mode (slot-value entry 'archive::mode)))
	    (unless (emptyp name)
	      (setf (gethash name result)
		    (cond
		      ((archive:entry-directory-p entry)
		       (list :directory mode))
		      ((archive:entry-regular-file-p entry)
		       (list :file
			     mode
			     (alexandria:read-stream-content-into-byte-vector
			      (archive:entry-stream entry))))
		      (t (error "Unsupported entry type.")))))))))
    result))
