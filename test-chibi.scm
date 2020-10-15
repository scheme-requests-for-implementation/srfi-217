(import (iset-trie)
        (chibi test)
        (only (srfi 1) iota)
        )

(define pos-set (list->iset (iota 20 100 3)))
(define neg-set (list->iset (iota 20 -100 3)))
(define mixed-set (list->iset (iota 20 -10 3)))

(test-group "comparison"
  (test #t (iset=? (iset) (iset)))
  (test #f (iset=? (iset 1) (iset)))
  (test #f (iset=? (iset) (iset 1)))
  (test #t (iset=? (iset 1 2 3 4) (iset 1 2 3 4)))
  (test #f (iset=? (iset 1 2 3 4) (iset 2 3 4)))
  (test #f (iset=? pos-set neg-set)))
