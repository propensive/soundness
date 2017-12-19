#!/bin/zsh

source ../build.zsh

PROJECT=probation
TEST=$1

build macros && \
build core -cp probation-macros.jar && \
build tests -cp probation-macros.jar:probation-core.jar && \
runtests tests -cp probation-macros.jar:probation-core.jar:probation-tests.jar
