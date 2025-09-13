#!/usr/bin/env bash

git ls-files -z '*.hs' | xargs -P 12 -0 -I file hlint file --refactor --refactor-options="--inplace --step"
