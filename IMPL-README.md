# iset-trie (SRFI 217)

This is an implementation of
[SRFI 217: Integer Sets](https://srfi.schemers.org/srfi-217)
for Scheme.  The implementation of integer sets is based on the
Patricia (radix) trie approach described in
["Fast Mergeable Integer Sets"](http://ittc.ku.edu/~andygill/papers/IntMap98.pdf)
by Chris Okasaki and Andrew Gill, with additional space-usage
optimizations inspired by Haskell's
[Data.IntSet](https://hackage.haskell.org/package/containers-0.6.4.1/docs/Data-IntSet.html)
library.  These tries provide fast lookup and set-theoretic
operations.

# Dependencies

This implementation should be portable to any R7RS Scheme.
The following libraries are used:

* [SRFI 1/(scheme list)](https://srfi.schemers.org/srfi-1)
* [SRFI 143/(scheme fixnum)](https://srfi.schemers.org/srfi-143)
* [SRFI 145](https://srfi.schemers.org/srfi-145) (optional)
* [SRFI 78](https://srfi.schemers.org/srfi-78) (for testing, optional)

# Author

Wolfgang Corcoran-Mathe

Contact: wcm at sigwinch dot xyzzy minus the zy
