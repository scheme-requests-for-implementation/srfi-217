(import (scheme base)
        (iset-trie)
        (chibi test)
        (only (srfi 1) iota any every last take-while drop-while)
        )

;;; Utility

(define (init xs)
  (if (null? (cdr xs))
      '()
      (cons (car xs) (init (cdr xs)))))

(define pos-seq (iota 20 100 3))
(define neg-seq (iota 20 -100 3))
(define mixed-seq (iota 20 -10 3))
(define sparse-seq (iota 20 -10000 1003))

(define pos-set (list->iset pos-seq))
(define neg-set (list->iset neg-seq))
(define mixed-set (list->iset mixed-seq))
(define dense-set (make-iset-range 0 49))
(define sparse-set (list->iset sparse-seq))

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

(test-group "Constructors"
  (test-equal iset=?
              (list->iset (iota 10 0 4))
              (iset-unfold (lambda (i) (> i 36))
                           values
                           (lambda (i) (+ i 4))
                           0))

  (test-equal iset=?
              (list->iset (iota 20 -10))
              (make-iset-range -10 10))
  )

(test-group "Predicates"
  (test-not (iset-contains? (iset) 1))
  (test-assert (every (lambda (n) (iset-contains? pos-set n))
                      (iota 20 100 3)))
  (test-assert (not (any (lambda (n) (iset-contains? pos-set n))
                         (iota 20 -100 3))))

  (test-assert (iset-empty? (iset)))
  (test-not (iset-empty? pos-set))

  (test-assert (iset-disjoint? (iset) (iset)))
  (test-assert (iset-disjoint? pos-set neg-set))
  (test-assert (iset-disjoint? (iset) pos-set))
  (test-not (iset-disjoint? dense-set sparse-set))
  (test-not (iset-disjoint? (make-iset-range 20 30) (make-iset-range 29 39)))
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

  (test-assert (iset-empty? (iset-delete-all (iset) '())))
  (test-equal iset=? pos-set (iset-delete-all pos-set '()))
  (test-equal iset=?
              (iset 100 103 106)
              (iset-delete-all pos-set (iota 17 109 3)))

  ;;; iset-delete-min / -max

  (test-values (values #f (iset)) (iset-delete-min (iset)))
  (test-values (values #t #t)
               (let-values (((n mixed-set*) (iset-delete-min mixed-set)))
                 (values (= n (car mixed-seq))
                         (iset=? mixed-set* (list->iset (cdr mixed-seq))))))
  (test-values (values #t #t)
               (let-values (((n sparse-set*) (iset-delete-min sparse-set)))
                 (values (= n (car sparse-seq))
                         (iset=? sparse-set* (list->iset (cdr sparse-seq))))))

  (test-values (values #f (iset)) (iset-delete-max (iset)))
  (test-values (values #t #t)
               (let-values (((n mixed-set*) (iset-delete-max mixed-set)))
                 (values (= n (last mixed-seq))
                         (iset=? mixed-set* (list->iset (init mixed-seq))))))
  (test-values (values #t #t)
               (let-values (((n sparse-set*) (iset-delete-max sparse-set)))
                 (values (= n (last sparse-seq))
                         (iset=? sparse-set* (list->iset (init sparse-seq))))))
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

  ;; iset-intersection
  (test-assert (iset-empty? (iset-intersection (iset) mixed-set)))
  (test-equal iset=? neg-set (iset-intersection neg-set neg-set))
  (test-equal iset=? (iset -97) (iset-intersection (iset -97) neg-set))
  (test-equal iset=? (iset) (iset-intersection pos-set neg-set))
  (test-equal iset=?
              (list->iset (drop-while negative? mixed-seq))
              (iset-intersection mixed-set dense-set))

  ;; iset-difference
  (test-assert (iset-empty? (iset-difference neg-set neg-set)))
  (test-equal iset=? pos-set (iset-difference pos-set neg-set))
  (test-equal iset=? pos-set (iset-difference pos-set neg-set))
  (test-equal iset=?
              (iset 100)
              (iset-difference pos-set (list->iset (cdr pos-seq))))
  (test-equal iset=?
              (list->iset (take-while negative? mixed-seq))
              (iset-difference mixed-set dense-set))

  ;; iset-xor
  (test-equal iset=? mixed-set (iset-xor (iset) mixed-set))
  (test-equal iset=?
              (list->iset (append (iota 20 100 3) (iota 20 -100 3)))
              (iset-xor pos-set neg-set))
  (test-equal iset=? (iset) (iset-xor pos-set pos-set))
  (test-equal iset=?
              (list->iset (append (iota 10 100 3) (iota 10 160 3)))
              (iset-xor pos-set (list->iset (iota 20 130 3))))
  )
