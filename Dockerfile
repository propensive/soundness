FROM ubuntu
ADD Makefile Makefile
RUN apt-get update
RUN apt-get install -y curl make
RUN make out/rudiments
