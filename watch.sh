#!/bin/bash
set -e

# GHCID_COMMAND='nix-shell --run "ghci -fobject-code -ferror-spans"'# ghcid -o ghcid.txt --command "${GHCID_COMMAND}"

emacsclient ./ghcid.txt &

nix-shell --run 'ghcid --reload="./sources/" --outputfile=./ghcid.txt --command "cabal repl memo"'
