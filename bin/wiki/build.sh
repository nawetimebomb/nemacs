#!/bin/sh

SCRIPT_DIR=$(cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd)

emacs -Q --script ${SCRIPT_DIR}/build-site.el
