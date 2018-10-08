#!/bin/bash

# Run Brittany on every Haskell source file and exit with error if the
# file is not identical to the output of Brittany.

mode="$1"
if [ "$mode" == "cached" ]; then
    cmd="git diff --cached --name-only"
else
    cmd='git ls-files'
fi

for file in `$cmd | grep "\\.hs$"`; do
    echo "Checking file $file"
    diff "$file" <(brittany --config-file brittany.yaml "$file") > /dev/null
    if [ "$?" != "0" ]; then
        echo "File $file is not formatted properly."
        diff -u "$file" <(brittany --config-file brittany.yaml "$file")
        exit 1
    fi
done

exit 0
