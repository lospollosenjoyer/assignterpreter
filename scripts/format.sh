#!/usr/bin/env bash

git ls-files -z '*.hs' | xargs -P 12 -0 fourmolu --mode inplace
