# The SRFI test collection

## The tests are

* Written in [SRFI 64](https://srfi.schemers.org/srfi-64/) syntax _(A
  Scheme API for test suites)_.
* Available under the MIT License, like the SRFIs themselves.

## Test sources

* SRFI sample implementations
* [Gauche](https://github.com/shirok/Gauche) by Shiro Kawai
* [Chibi-Scheme](https://github.com/ashinn/chibi-scheme) by Alex Shinn

## Usage

### Convert the tests

`convert.scm` is a portable Scheme program that converts all tests
into a form suitable for each Scheme implementation, R6RS and R7RS. Each
implementation and report version gets its own subdirectory where the tests go.
To create or update the subdirectories based on the `.scm` files in the root
directory, run one of the following:

    chibi-scheme convert.scm
    csi convert.scm
    gosh convert.scm
    guile convert.scm
    kawa convert.scm

### Run the tests

Then run the tests for one or more implementations.
You need to match the right subdirectory with the right Scheme implementation
or report version. Example for SRFI 64:

    chibi-scheme chibi/64.scm
    csi chicken/64.scm
    gosh gauche/64.scm
    guile guile/64.scm
    kawa kawa/64.scm

    chezscheme r6rs-programs/64.scm
    chibi-scheme r7rs-programs/64.scm
    csi r7rs-programs/64.scm
    gosh r7rs-programs/64.scm
    guile r7rs-programs/64.scm
    kawa r7rs-programs/64.scm

If you want Test Anything Protocol (TAP) output install (retropikzel tap) and
run the tap- prefixed versions, currently only r7rs-programs have these.

    chibi-scheme r7rs-programs/tap-64.scm

## Contributing new tests

Tests for new SRFIs, as well as new tests for already covered SRFIs,
are very welcome as long as they use SRFI 64 syntax and are MIT-licensed.

Add file N.scm which should contain the tests. Add the SRFI number into
all-srfis list inside convert.scm. It's near the end of the file.
And if necessary you can change the imports of your tests by adding a new cond
branch inside the r6rs-imports and r7rs-imports variables.
