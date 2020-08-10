Herbie support for machine integers
===

This repository contains a plugin for [Herbie](https://herbie.uwplse.org) to support machine integers.

So far, only signed 64-bit integers (int64_t) are supported.
Use in Herbie by specifying `:precision integer`

This package contains:

+ Definitions of machine integers and their operators for Herbie
+ Mixed integer/float functions from libm
+ Rewrite rules for these operators

The best way to install this package is using the Racket package manager:

    raco pkg install integer-herbie