#!/bin/sh
erl -sname fipes@localhost \
    -pa fipes/ebin         \
    -pa deps/*/ebin        \
    -boot start_sasl       \
    -s fipes               \
    -detached
