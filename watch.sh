#!/bin/bash
GHCID_COMMAND='ghci -fobject-code -ferror-spans -ignore-dot-ghci' 
ghcid -o ghcid.txt --command "${GHCID_COMMAND}"
