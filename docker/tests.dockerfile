#
# Build on ubuntu and run some tests
#

FROM ubuntu:latest as build

RUN apt-get update
RUN apt-get install -y sbcl curl

RUN curl https://beta.quicklisp.org/quicklisp.lisp > quicklisp.lisp

RUN sbcl --noinform --load quicklisp.lisp \
         --eval '(quicklisp-quickstart:install)'

RUN sbcl --noinform --load ~/quicklisp/setup.lisp \
         --eval '(ql:quickload (quote (:ironclad :alexandria \
          :flexi-streams)))'

WORKDIR /root/quicklisp/local-projects/dill
COPY . .
RUN ./compile.sh

FROM ubuntu:latest

RUN apt-get update
RUN apt-get install -y git

WORKDIR /root

COPY --from=build /root/quicklisp/local-projects/dill/dill /bin/dill
COPY integration-tests.sh .
RUN ./integration-tests.sh
