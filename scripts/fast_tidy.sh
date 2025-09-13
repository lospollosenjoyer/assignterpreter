#!/usr/bin/env bash

echo "> Formatting..."
git ls-files -z '*.hs' | xargs -P 12 -0 fourmolu --mode inplace

echo "> Linting..."
git ls-files -z '*.hs' | xargs -P 12 -0 -I file hlint file --refactor --refactor-options="--inplace"
