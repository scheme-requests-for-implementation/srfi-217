(import (iset-trie)
        (chibi test)
        (only (srfi 1) iota every)
        )

(define pos-set (list->iset (iota 20 100 3)))
(define neg-set (list->iset (iota 20 -100 3)))
(define mixed-set (list->iset (iota 20 -10 3)))
(define dense-set (make-iset-range 0 49))
(define sparse-set (list->iset (iota 20 -10000 1003)))

(define all-test-sets
  (list pos-set neg-set mixed-set dense-set sparse-set))

(test-group "comparison"
  (test #t (iset=? (iset) (iset)))
  (test #f (iset=? (iset 1) (iset)))
  (test #f (iset=? (iset) (iset 1)))
  (test #t (iset=? (iset 1 2 3 4) (iset 1 2 3 4)))
  (test #f (iset=? (iset 1 2 3 4) (iset 2 3 4)))
  (test #f (iset=? pos-set neg-set)))

(test-group "copying and conversion"
  ;;; iset-copy 
  (test-assert (not (eqv? (iset-copy pos-set) pos-set)))
  (test-assert (every (lambda (set)
                        (iset-every? (lambda (n) (iset-contains? set n))
                                     (iset-copy set)))
                      all-test-sets))

  ;;; iset->list

  (test '() (iset->list (iset)))
  (test '(0) (iset->list (iset 0)))
  (test-assert (= (length (iset->list pos-set)) (iset-size pos-set)))
  (test-assert (every (lambda (n) (iset-contains? pos-set n))
                      (iset->list pos-set)))
  )
