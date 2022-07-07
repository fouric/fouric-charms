#!/bin/sh

# reminder: check your color scheme if you're getting gray backgrounds!

# https://superuser.com/questions/361902/continuously-re-execute-a-command-when-it-finishes-in-bash

export TERM=xterm

/usr/bin/sbcl --noinform --load dev.lisp
