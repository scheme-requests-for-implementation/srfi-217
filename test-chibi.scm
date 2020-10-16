(import (scheme base)
        (iset-trie)
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

(test-group "updaters"
  (test '(1) (iset->list (iset-adjoin (iset) 1)))
  (test-assert (iset-contains? (iset-adjoin neg-set 10) 10))
  (test-assert (iset-contains? (iset-adjoin dense-set 100) 100))
  (test-assert (iset-contains? (iset-adjoin sparse-set 100) 100))
  (test-equal iset=?
              (list->iset (cons -3 (iota 20 100 3)))
              (iset-adjoin pos-set -3))

  (test '() (iset->list (iset-delete (iset 1) 1)))
  (test-not (iset-contains? (iset-delete neg-set 10) 10))
  (test-not (iset-contains? (iset-delete dense-set 1033) 1033))
  (test-not (iset-contains? (iset-delete sparse-set 30) 30))
  (test-equal iset=?
              (list->iset (cdr (iota 20 100 3)))
              (iset-delete pos-set 100))
  )

(test-group "set theory"
  (test-equal iset=? mixed-set (iset-union! (iset) mixed-set))
  (test-equal iset=?
              (list->iset (append (iota 20 100 3) (iota 20 -100 3)))
              (iset-union pos-set neg-set))
  (test-equal iset=? pos-set (iset-union pos-set pos-set))
  (test-equal iset=?
              (list->iset (iota 30 100 3))
              (iset-union pos-set (list->iset (iota 20 130 3))))
  )
