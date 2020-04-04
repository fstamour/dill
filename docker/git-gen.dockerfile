# This dockerfile is used to generate simple git repository for testing

FROM alpine:3.11

# Install and setup git
RUN apk add --no-cache git
RUN git config --global user.name "User Name" \
 && git config --global user.email "example@example.com"

# Setup workdir
RUN mkdir -p /repository
RUN mkdir -p /build_outputs
WORKDIR /repository

# Create an empty repository and take a "snapshot"
RUN git init
RUN tar -czvf /build_outputs/empty.tgz .

RUN touch readme && git add readme
RUN tar -czvf /build_outputs/empty-with-staged-readme.tgz .

RUN git commit -m "add empty readme"
RUN tar -czvf /build_outputs/empty-readme.tgz .

ENTRYPOINT [ "sh", "-c", "cp /build_outputs/* /target" ]
