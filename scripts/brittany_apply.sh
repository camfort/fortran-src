#!/bin/bash

# Run Brittany on every Haskell source file and exit with error if the
# file cannot be handled correctly.

mode="$1"
if [ "$mode" == "cached" ]; then
    cmd="git diff --cached --name-only"
else
    cmd='git ls-files'
fi

for file in `$cmd | grep "\\.hs$"`; do
    echo "Reformatting file $file"
    brittany --config-file brittany.yaml --write-mode inplace "$file"
    if [ "$?" != "0" ]; then
        echo "File $file caused error."
        exit 1
    fi
done

exit 0
