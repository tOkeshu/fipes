#!/bin/sh
erl -sname fipes@localhost \
    -pa ebin         \
    -pa deps/*/ebin        \
    -boot start_sasl       \
    -s fipes

