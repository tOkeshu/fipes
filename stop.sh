#!/bin/sh
erl -sname fipectl@localhost \
    -pa fipes/ebin           \
    -pa deps/*/ebin          \
    -eval "fipes:shutdown(fipes@localhost)" \
    -s init stop \
    -detached \
