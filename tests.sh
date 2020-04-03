#!/bin/sh

sbcl --load ~/quicklisp/setup.lisp \
     --eval "(ql:quickload '(:dill :dill.test))" \
     --script run-tests.lisp
