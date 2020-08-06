;;;; parse git's configuration file
;;;;
;;;; I tried the library "py-configparser" but it would crash
;;;; when there was elading whitespaces.
;;;;
;;;; This parser works well, but it reads the whole file at once.


(in-package #:dill)

;; This is from alexa's documentation
(defun lex-line (string make-lexer-fn)
  "Tokenize a string. Takes a string and a lexer-constructor."
  (loop :with lexer := (funcall make-lexer-fn string)
     :for tok := (funcall lexer)
     :while tok
     :collect tok))

(alexa:define-string-lexer config-lexer
    ()
    ;; ignore whitespaces
    ("\\s+" nil)
    ;; ignore comments
    ("#.*" nil)
    ;; equal
    ("=" (return :=))
    ;; parse integers
    ("[\\d]+" (return (parse-integer $@)))
    ;; parse booleans (case-sensitive)
    ("true" (return :true))
    ("false" (return :false))
    ;; extract section name
    ("\\[(?<NAME>[^\\]]+)\\]" (return $NAME))
    ;; anything else is as-is
    ("[^\\s]+" (return $@)))

(defun parse-config (string
		     &optional (config
				(make-hash-table :test 'equal)))
  "Parse a config file, returns a nested (2 level) hash-table.
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

(defun parse-ref (string)
  (second
   (split-sequence:split-sequence
    #\space string
    :remove-empty-subseqs t)))
