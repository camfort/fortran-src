#!/bin/bash

full="`readlink -f \"$0\"`"
dir="`dirname \"$full\"`"
cd $dir/..

[ ! -d .git ] && echo "Error: unable to find git directory." && exit 1

ln -s ../../git_hooks/pre-commit .git/hooks/ && echo "Installed pre-commit hook."
