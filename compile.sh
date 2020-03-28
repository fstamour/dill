#!/bin/sh

sbcl --load ~/quicklisp/setup.lisp \
     --load git.lisp \
     --script make-executable.lisp
