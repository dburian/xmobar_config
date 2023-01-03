#!/bin/bash

dir="$(dirname $0)"

cabal update
rm -f .ghc.environment.*

echo Instaling xmobar to \'"$dir"\'
cabal install --package-env="$dir" --lib --flags="all_extensions" --overwrite-policy=always xmobar
# cabal install --package-env="$dir" --flags="all_extensions" --overwrite-policy=always xmobar
