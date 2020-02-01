# The SRFI test collection

## The tests are

* Written in SRFI 64 syntax _(A Scheme API for test suites)_.
* Available under the MIT License, like the SRFIs themselves.

## Test sources

* SRFI sample implementations
* [Gauche](https://github.com/shirok/Gauche) by Shiro Kawai
* [Chibi-Scheme](https://github.com/ashinn/chibi-scheme) by Alex Shinn

## Usage

### Convert the tests

`convert.scm` is a portable Scheme program that converts all tests
into a form suitable for each Scheme implementation. Each
implementation gets its own subdirectory where the tests go. To create
or update the subdirectories based on the `.scm` files in the root
directory, run one of the following:

    chibi-scheme convert.scm
    csi convert.scm
    gosh convert.scm
    guile convert.scm
    kawa convert.scm

### Run the tests

Then run the tests for one or more implementations. Unlike the
converter, the tests are generally not portable, and you need to match
the right subdirectory with the right Scheme implementation. Example
for SRFI 1:

    chibi-scheme chibi/1.scm
    csi chicken/1.scm
    gosh gauche/1.scm
    guile guile/1.scm
    kawa kawa/1.scm

## Contributing new tests

Tests for new SRFIs, as well as new tests for already covered SRFIs,
are very welcome as long as they use SRFI 64 syntax and are
MIT-licensed.
