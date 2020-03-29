
(defpackage #:dill.asd
  (:use :cl :asdf)
  (:export #:project-name))

(in-package #:dill.asd)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar project-name :git))

(asdf:defsystem #:dill
  :description "Implementation of git purely in common lisp"
  :version "0.1.0"
  :author "Francis St-Amour"
  :licence "GNU GPLv3"
  :depends-on (#:access		       ; dig
	       #:alexandria	       ; de-facto standard utilities
	       #:alexa		       ; lexer generator
	       #:apply-argv	       ; command-line argument parsing
	       #:cl-fad		       ; paths, files and folders
	       #:flexi-streams	       ; octets <-> strings
	       #:split-sequence
	       #:optima			; pattern matching
	       #:track-best				
	       #:vas-string-metrics	; string distance
	       #:zlib)
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
