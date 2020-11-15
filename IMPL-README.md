# iset-trie

This is an implementation of integer sets for Scheme after a
[pre-SRFI by John Cowan](https://github.com/johnwcowan/r7rs-work/blob/master/IntegerSetsCowan.md).
The implementation is based on the Patricia (radix) trie approach
described in
["Fast Mergeable Integer Sets"](http://ittc.ku.edu/~andygill/papers/IntMap98.pdf)
by Chris Okasaki and Andrew Gill.

# Limitations

Currently, this implementation supports only sets of fixnums (using
[SRFI 143](https://srfi.schemers.org/srfi-143/srfi-143.html)).  This may
change in the future.

# Future directions

"Buddy compression" using bitmaps could reduce the space usage of a set.

# Author

Wolfgang Corcoran-Mathe

Contact: wcm at sigwinch dot xyzzy minus the zy
