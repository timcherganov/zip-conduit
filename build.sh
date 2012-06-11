#!/bin/bash

# Install dependencies with profiling options.
cabal-dev install --only-dependencies --enable-tests --enable-benchmarks \
                  --enable-library-profiling --enable-executable-profiling

# Configure and build with testing and code coverage options.
cabal-dev configure --enable-tests --enable-library-coverage \
                    --enable-benchmarks
cabal-dev build

#cabal-dev test
#cabal-dev bench --benchmark-option="-o bench.html"
#cabal-dev haddock
