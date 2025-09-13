#!/usr/bin/env bash

echo "> Format checking..."
git ls-files -z '*.hs' | xargs -P 12 -0 fourmolu --mode check

echo "> Linter checking..."
git ls-files -z '*.hs' | xargs -P 12 -0 hlint
