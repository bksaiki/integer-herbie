Herbie support for machine integers
===

This repository contains a plugin for [Herbie](https://herbie.uwplse.org) to support machine integers.

So far, only signed 32-bit integers are supported. Use in Herbie by specifying `:precision integer`.

This package contains:

+ Definitions of machine integers and their operators for Herbie
+ Mixed integer/float functions from libm
+ Rewrite rules for these operators

Installation:

1. Clone this repository, preferably into a folder called `integer-herbie`.
2. If the folder name is `integer-herbie`, run `raco pkg install` within the folder.
   Otherwise, run `raco pkg install -n integer-herbie`.