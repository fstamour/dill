#!/bin/sh

# stop on first error
set -e

if [ -d build_outputs ]; then
  rm -r build_outputs
fi
mkdir -p build_outputs

dockerfile=git-gen.dockerfile
tag=$dockerfile

# build the image
docker build -t $tag -f $dockerfile .

# create the container
containerId=$(docker create $tag)
echo Container ID: $containerId

# copy the build_outputs from the container
docker cp ${containerId}:/build_outputs build_outputs/

# remove the container
docker rm $containerId
