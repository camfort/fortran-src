#!/bin/bash

# Run Brittany on every Haskell source file and exit with error if the
# file is not identical to the output of Brittany.

set -ev

for file in `find . -name '*.hs' -print`; do
    echo "Checking file=$file"
    diff <(brittany --config-file brittany.yaml "$file") "$file" > /dev/null
done
