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
          iset? iset-contains?
          iset-empty? iset-disjoint?
          iset-adjoin iset-adjoin! iset-delete iset-delete! iset-delete-all
          iset-delete-all!
          iset-trie  ; debug
          )

  (include "iset-trie.scm"))
