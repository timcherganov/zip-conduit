#!/bin/bash

# Install with benchmarking, testing, code coverage and profiling options.
cabal-dev install --reinstall --enable-benchmarks \
                  --enable-tests --enable-library-coverage \
                  --enable-library-profiling --enable-executable-profiling


# It seems that 'cabal-dev install' don't work with --enable-benchmarks
cabal-dev configure --enable-benchmarks \
                    --enable-tests --enable-library-coverage \
                    --enable-library-profiling --enable-executable-profiling
cabal-dev build

#cabal-dev test
#cabal-dev bench --benchmark-option="-obench.html"
#cabal-dev haddock
