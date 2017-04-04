[![Build Status](https://travis-ci.org/klangner/radium.svg?branch=master)](https://travis-ci.org/klangner/radium)

# Introduction

Radium is Haskell library for the Chemistry.
It has the following functionality:
* Periodic table with the element data.
* Readers and writers for the following formats:
  * SMILES ([examples](https://github.com/klangner/radium/blob/master/test-src/Radium/Formats/SmilesSpec.hs)).
  * Condensed ([examples](https://github.com/klangner/radium/blob/master/test-src/Radium/Formats/CondensedSpec.hs)).


## Installation
The library can be installed from [Hackage](http://hackage.haskell.org/package/radium) with the command:

```sh
stack setup
stack build
stack test
```

## Redistributing

radium source code is distributed under the BSD3 License.
