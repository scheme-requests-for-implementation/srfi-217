(define-library (iset-trie)
  (import (scheme base)
          (only (srfi 1) fold)
          (srfi 143))

  (cond-expand
    ((library (srfi 145))
     (import (srfi 145)))
    (else
     (begin
      (define-syntax assume
        (syntax-rules ()
          ((_ expr . _)
           (or expr (car 0))))))))

  (export iset list->iset
          iset-unfold make-iset-range
          iset-min iset-max
          iset? iset-contains?
          iset-empty? iset-disjoint?
          iset-adjoin iset-adjoin! iset-delete iset-delete! iset-delete-all
          iset-delete-all!
          iset-delete-min iset-delete-max
          iset-delete-min! iset-delete-max!
          iset-size
          iset-any? iset-every?
          iset-count iset-fold
          iset-map iset-for-each
          iset-filter iset-remove
          iset-copy
          iset->list
          iset=?
          iset<? iset>? iset<=? iset>=?
          iset-union iset-union!
          iset-intersection iset-intersection!
          iset-difference iset-difference!
          iset-xor iset-xor!
          iset-trie  ; debug
          )

  (include "trie.scm")
  (include "iset-trie.scm"))
