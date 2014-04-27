# streams README

## Description

streams is an adaptation of Phil Bewig's reference implementation of srfi-41, a streams (i.e., lazy lists) library, for Bigloo. The documentation for srfi-41 can be found at <http://srfi.schemers.org/srfi-41/srfi-41.html>.

### Building

Both the Bigloo native and jvm backends are supported. To build the native libraries, just execute

    make

To build the jvm libraries, execute

	make jvm

To build both, execute

	make all


### Installation

To Install the libraries, execute

	make install

This by default installs the libraries into the directory /usr/lib/bigloo/<bigloo-version>. If you have Bigloo installed to a different prefix, execute

	make install DESTDIR=/path/prefix

This will result in the libraries being installed to /path/prefix/lib/bigloo/<bigloo-version>.

### Interpretor

The bkanren library can be dynamically loaded into the interpretor with

	(library-load 'streams)

If you have not installed the bkanren library in a standard location, you may need to pass path as a second argument.

	(libary-load 'streams "/path/to/libdir")

