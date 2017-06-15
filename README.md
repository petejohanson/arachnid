# Arachnid Web Framework

[![CircleCI](https://img.shields.io/circleci/project/github/petejohanson/arachnid.svg)](https://circleci.com/gh/petejohanson/arachnid) [![Coverage Status](https://coveralls.io/repos/github/petejohanson/arachnid/badge.svg?branch=master)](https://coveralls.io/github/petejohanson/arachnid?branch=master)

### WARNING

This is a work-in-progress! It does *not* fully work. If it breaks, you get to keep both pieces.

## To Do

* Custom state flowing through decision tree
* Response encoding
* Standard state flowing through the decision tree (e.g. headers? Selected media type? Response body?)
* Better example(s)
* Docs

### Some thoughts.

Move decisions to decision "registry", e.g.:

data DecisionNode = O12 | O15 | .. | M20 deriving (Show)

decision :: (Resource a) -> DecisonNode -> ResourceMonad DecisionNode

decision O12 = decideIfBranch resourceExists O15 O17

Can we use this to create a tracing executor versus production one?

### State monad for response data (headers, code, anything else?)

* How to handle polymorphism of state? Existencial types + wwrapper type?
* Where to store trace data?


