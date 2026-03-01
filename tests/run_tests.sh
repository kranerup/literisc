#!/usr/bin/env bash
for f in *.c; do
    if ../lrcc.lisp -r $f ; then
       echo test $f ok
    else
       echo test $f FAIL
    fi
done

