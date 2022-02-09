#!/bin/bash

if [ -z "$1" ]; then
  echo 'missing source directory'
  exit 1
fi

if [ -z "$2" ]; then
  echo 'missing target directory'
  exit 1
fi

dune build index/index.exe

find "$1" -name '*.ml*' \
  | sort -R \
  | dune exec -- index/index.exe "$1" "$2"
