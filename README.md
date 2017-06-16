# Arachnid Web Framework

[![CircleCI](https://img.shields.io/circleci/project/github/petejohanson/arachnid.svg)](https://circleci.com/gh/petejohanson/arachnid) [![Coverage Status](https://coveralls.io/repos/github/petejohanson/arachnid/badge.svg?branch=master)](https://coveralls.io/github/petejohanson/arachnid?branch=master)

### WARNING

This is a work-in-progress! It does *not* fully work. If it breaks, you get to keep both pieces.

## To Do

* Change Resource methods to return `data ResourceResult = Error HTTP.Status | Halt HTTP.Status | Value a` so functions like `decisionBranch` can short-circuit!
* Custom state flowing through decision tree
* Response encoding
* Better example(s)
* Docs
* Graceful shutdown in example?

### State monad for response data (headers, code, anything else?)

* How to handle polymorphism of state? Existencial types + wwrapper type?
* Where to store trace data?


