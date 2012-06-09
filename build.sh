#!/bin/bash

# Install dependencies with profiling options.
cabal-dev install --only-dependencies --enable-tests --enable-library-profiling --enable-executable-profiling

# Configure and build with testing and code coverage options.
cabal-dev configure --enable-tests --enable-library-coverage
cabal-dev build

cabal-dev test
cabal-dev haddock
