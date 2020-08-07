#!/bin/sh

docker build -f tests.dockerfile -t dill ..
docker run -it --rm dill
