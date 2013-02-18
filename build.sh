#!/bin/bash

# Install all dependencies (for library, tests and benchmarks) with
# profiling enabled.
cabal-dev install \
    --only-dependencies \
    --enable-benchmarks \
    --enable-tests \
    --enable-library-profiling

# Configure and build.
cabal-dev configure \
    --enable-benchmarks \
    --enable-tests \
    --enable-library-coverage \
    --enable-library-profiling
cabal-dev build

cabal-dev test
#cabal-dev bench --benchmark-option="-obench.html"
#cabal-dev haddock
