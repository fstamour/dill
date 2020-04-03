#!/bin/sh

sbcl --load ~/quicklisp/setup.lisp \
     --eval "(ql:quickload '(:dill))" \
     --script make-executable.lisp
