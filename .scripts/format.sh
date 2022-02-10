#!/bin/sh

cd "$(dirname "$0")" || exit

find ../* -name '*.hs' -exec \
  brittany \
    --ghc-options "$(yq -r  '."default-extensions"|map("-X"+.)|join(" ")' ../package.yaml)" \
    --write-mode inplace \
    {} \
  \;

find ../* -name '*.py' -exec \
  black \
    {} \
  \;
