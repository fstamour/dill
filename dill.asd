
(defpackage #:dill.asd
  (:use :cl :asdf)
  (:export #:project-name))

(in-package #:dill.asd)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar project-name :git))

(asdf:defsystem #:dill
  :description "Implementation of git purely in common lisp"
  :version "0.0.1"
  :author "Francis St-Amour"
  :licence "GNU GPLv3"
  :depends-on (#:access		       ; dig into structures
	       #:alexandria	       ; de-facto standard utilities
	       #:alexa		       ; lexer generator
	       #:apply-argv	       ; command-line argument parsing
	       #:archive	       ; to read tar archive
	       #:cl-fad		       ; paths, files and folders
	       #:flexi-streams	       ; octets <-> strings
	       #:gzip-stream	       ; to read gzip file
	       #:split-sequence
	       #:optima			; pattern matching
	       #:vas-string-metrics	; string distance
	       #:zlib)
  :in-order-to ((asdf:test-op (asdf:test-op :dill.test)))
  :serial t
  :components
  ((:module "src"
	    :components
	    ((:file "package")
	     (:file "utils")
	     (:file "git-repository")	; class "git-repository"
	     (:file "config-parser")
	     (:file "rfc2822-parser") ; to parse commits, trees and tags
	     (:file "commands")	      ; command-line commands
	     (:file "git")))
   (:module "tests"
	    :components ())))
