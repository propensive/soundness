#!/bin/zsh

source ../build.zsh

PROJECT=estrapade
TEST=$1

build macros && \
build core -cp estrapade-macros.jar && \
build tests -cp estrapade-macros.jar:estrapade-core.jar && \
runtests tests -cp estrapade-macros.jar:estrapade-core.jar:estrapade-tests.jar
