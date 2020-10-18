;;; FIXME:
;;;
;;; * Bitmap compression.  Dense trees waste space at the moment.
;;;
;;; * Too much duplication of the trie-traversing algorithm
;;;   with minor variations.  Unify procedures where possible.

(define-record-type <branch>
  (branch prefix branching-bit left right)
  branch?
  (prefix branch-prefix)
  (branching-bit branch-branching-bit)
  (left branch-left)
  (right branch-right))

;; Shorthand for extracting branch elements.
(define-syntax let*-branch
  (syntax-rules ()
    ((_ () e1 e2 ...) (begin e1 e2 ...))
    ((_ (((p m l r) expr) . binds) . body)
     (let ((b expr))
       (let ((p (branch-prefix b))
             (m (branch-branching-bit b))
             (l (branch-left b))
             (r (branch-right b)))
         (let*-branch binds . body))))))

(define (valid-integer? x) (fixnum? x))

;; Zero the bits of k at and below (BE) the set bit of m.
(define (mask k m)
  (if (fx=? m fx-least)
      0
      (fxand k (fxxor (fxnot (fx- m 1)) m))))

;; Does the m-masked prefix of k match p?
(define (match-prefix? k p m)
  (fx=? (mask k m) p))

(define (branching-bit p1 m1 p2 m2)
  (if (fxnegative? (fxxor p1 p2))
      fx-least        ; different signs
      (highest-bit-mask (fxxor p1 p2) (fxmax 1 (fx* 2 (fxmax m1 m2))))))

;; Two's-complement trick.
(define (lowest-set-bit b)
  (fxand b (fxneg b)))

(define (highest-bit-mask k guess-m)
  (let lp ((x (fxand k (fxnot (fx- guess-m 1)))))
    (let ((m (lowest-set-bit x)))
      (if (fx=? x m)
          m
          (lp (fx- x m))))))

(define (zero-bit? k m)
  (fxzero? (fxand k m)))

(define (trie-insert trie key)
  (letrec
   ((ins
     (lambda (t)
       (cond ((not t) key)  ; new leaf
             ((integer? t)
              (if (fx=? t key) t (trie-join key 0 key t 0 t)))
             (else
              (let*-branch (((p m l r) t))
                (if (match-prefix? key p m)
                    (if (zero-bit? key m)
                        (branch p m (ins l) r)
                        (branch p m l (ins r)))
                    (trie-join key 0 key p m t))))))))
    (ins trie)))

(define (trie-join prefix1 mask1 trie1 prefix2 mask2 trie2)
  (let ((m (branching-bit prefix1 mask1 prefix2 mask2)))
    (if (zero-bit? prefix1 m)
        (branch (mask prefix1 m) m trie1 trie2)
        (branch (mask prefix1 m) m trie2 trie1))))

(define (trie-contains? trie key)
  (and trie
       (if (integer? trie)
           (fx=? key trie)
           (let*-branch (((p m l r) trie))
             (and (match-prefix? key p m)
                  (if (zero-bit? key m)
                      (trie-contains? l key)
                      (trie-contains? r key)))))))

(define (branching-bit-higher? mask1 mask2)
  (if (negative? (fxxor mask1 mask2))  ; signs differ
      (negative? mask1)
      (fx>? mask1 mask2)))

(define (trie-merge insert trie1 trie2)
  (letrec
    ((merge
      (lambda (s t)
        (cond ((not s) t)
              ((not t) s)
              ((integer? s) (insert t s))
              ((integer? t) (insert s t))
              (else (merge-branches s t)))))
     (merge-branches
      (lambda (s t)
        (let*-branch (((p m s1 s2) s)
                      ((q n t1 t2) t))
          (cond ((and (fx=? m n) (fx=? p q))
                 ;; the prefixes match, so merge the subtries
                 (smart-branch p m (merge s1 t1) (merge s2 t2)))
                ((and (branching-bit-higher? m n) (match-prefix? q p m))
                 ;; p is a prefix of q, so merge t with a subtrie of s.
                 (if (zero-bit? q m)
                     (smart-branch p m (merge s1 t) s2)
                     (smart-branch p m s1 (merge s2 t))))
                ((and (branching-bit-higher? n m) (match-prefix? p q n))
                 ;; q is a prefix of p, so merge s with a subtrie of t.
                 (if (zero-bit? p n)
                     (smart-branch q n (merge s t1) t2)
                     (smart-branch q n t1 (merge s t2))))
                (else    ; the prefixes disagree
                 (trie-join p m s q n t)))))))
    (merge trie1 trie2)))

;; Construct a branch only if the subtrees are non-empty.
(define (smart-branch prefix mask trie1 trie2)
  (cond ((not trie1) trie2)
        ((not trie2) trie1)
        (else (branch prefix mask trie1 trie2))))

(define (copy-trie trie)
  (and trie
       (if (integer? trie)
           trie
           (branch (branch-prefix trie)
                   (branch-branching-bit trie)
                   (copy-trie (branch-left trie))
                   (copy-trie (branch-right trie))))))

(define (trie-filter pred trie)
  (and trie
       (if (integer? trie)
           (and (pred trie) trie)
           (smart-branch (branch-prefix trie)
                         (branch-branching-bit trie)
                         (trie-filter pred (branch-left trie))
                         (trie-filter pred (branch-right trie))))))

(define (trie-remove pred trie)
  (and trie
       (if (integer? trie)
           (and (not (pred trie)) trie)
           (smart-branch (branch-prefix trie)
                         (branch-branching-bit trie)
                         (trie-remove pred (branch-left trie))
                         (trie-remove pred (branch-right trie))))))

(define (%trie-find-leftmost trie)
  (if (or (not trie) (integer? trie))
      trie
      (%trie-find-leftmost (branch-left trie))))

(define (%trie-find-rightmost trie)
  (if (or (not trie) (integer? trie))
      trie
      (%trie-find-rightmost (branch-right trie))))

;;;; Comparisons

(define (trie=? trie1 trie2)
  (cond ((not (or trie1 trie2)) #t)
        ((and (integer? trie1) (integer? trie2)) (fx=? trie1 trie2))
        ((and (branch? trie1) (branch? trie2))
         (let*-branch (((p m l1 r1) trie1) ((q n l2 r2) trie2))
           (and (fx=? m n) (fx=? p q) (trie=? l1 l2) (trie=? r1 r2))))
        (else #f)))

;; Returns the symbol 'less' if trie1 is a proper subset of trie2,
;; 'equal' if they are the same, and 'greater' otherwise.  NB that
;; disjoint sets will compare as greater.
;;
;; FIXME: Simplify this.
(define (trie-subset-compare trie1 trie2)
  (letrec
   ((compare
     (lambda (s t)
       (cond ((eqv? s t) 'equal)
             ((not s) 'less)
             ((not t) 'greater)  ; disjoint
             ((and (integer? s) (integer? t))
              (if (fx=? s t) 'equal 'greater))
             ((integer? s)             ; leaf / branch
              (let*-branch (((p m l r) t))
                (if (match-prefix? s p m)
                    (let ((res (compare s (if (zero-bit? s m)
                                              (branch-left t)
                                              (branch-right t)))))
                      (if (eqv? res 'greater) res 'less)))))
             ((integer? t) 'greater)   ; branch / leaf
             (else (compare-branches s t)))))
    (compare-branches
     (lambda (s t)
       (let*-branch (((p m sl sr) s) ((q n tl tr) t))
         (cond ((branching-bit-higher? m n) 'greater)
               ((branching-bit-higher? n m)
                (if (match-prefix? p q n)
                    (let ((comp (if (zero-bit? p n)
                                    (compare s tl)
                                    (compare s tr))))
                      (if (eqv? comp 'greater) comp 'less))
                    'greater))
               ((fx=? p q)  ; same prefix, compare subtrees
                (let ((cl (compare sl tl)) (cr (compare sr tr)))
                  (cond ((or (eqv? cl 'greater) (eqv? cr 'greater))
                         'greater)
                        ((and (eqv? cl 'equal) (eqv? cr 'equal))
                         'equal)
                        (else 'less))))
               (else 'greater))))))  ; disjoint
    (compare trie1 trie2)))

(define (trie-proper-subset? trie1 trie2)
  (eqv? (trie-subset-compare trie1 trie2) 'less))

(define (trie-disjoint? trie1 trie2)
  (letrec
   ((disjoint?
     (lambda (s t)
       (or (not s)
           (not t)
           (cond ((integer? s)
                  (if (integer? t)
                      (not (fx=? s t))
                      (not (trie-contains? t s))))
                 ((integer? t) (not (trie-contains? s t)))
                 (else (branches-disjoint? s t))))))
    (branches-disjoint?
     (lambda (s t)
       (let*-branch (((p m sl sr) s) ((q n tl tr) t))
         (cond ((and (fx=? m n) (fx=? p q))
                (and (disjoint? sl tl) (disjoint? sr tr)))
               ((and (branching-bit-higher? m n) (match-prefix? q p m))
                (if (zero-bit? q m)
                    (disjoint? sl t)
                    (disjoint? sr t)))
               ((and (branching-bit-higher? n m) (match-prefix? p q n))
                (if (zero-bit? p n)
                    (disjoint? s tl)
                    (disjoint? s tr)))
               (else #t))))))      ; the prefixes disagree
    (disjoint? trie1 trie2)))

(define (trie-delete trie key)
  (letrec
   ((update
     (lambda (t)
       (cond ((not t) #f)
             ((integer? t) (if (fx=? t key) #f t))
             (else (update-branch t)))))
    (update-branch
     (lambda (t)
       (let*-branch (((p m l r) t))
         (if (match-prefix? key p m)
             (if (zero-bit? key m)
                 (smart-branch p m (update l) r)
                 (smart-branch p m l (update r)))
             t)))))  ; key doesn't occur in t
    (update trie)))

;; Identical to trie-insert, but delete key if it exists.
(define (trie-xor-insert trie key)
  (letrec
   ((ins
     (lambda (t)
       (cond ((not t) key)  ; new leaf
             ((integer? t)
              (if (fx=? t key) #f (trie-join key 0 key t 0 t)))
             (else
              (let*-branch (((p m l r) t))
                (if (match-prefix? key p m)
                    (if (zero-bit? key m)
                        (smart-branch p m (ins l) r)
                        (smart-branch p m l (ins r)))
                    (trie-join key 0 key p m t))))))))
    (ins trie)))

(define (trie-intersection trie1 trie2)
  (letrec
   ((intersect
     (lambda (s t)
       (cond ((or (not s) (not t)) #f)
             ((and (integer? s) (integer? t)) (and (fx=? s t) s))
             ((integer? s) (ins-int s t))
             ((integer? t) (ins-int t s))
             (else (intersect-branches s t)))))
    (ins-int
     (lambda (n t)
       (let lp ((t t))
         (cond ((and (integer? t) (fx=? n t)) n)
               ((branch? t)
                (let*-branch (((p m l r) t))
                  (and (match-prefix? n p m)
                       (if (zero-bit? n m) (lp l) (lp r)))))
               (else #f)))))
    (intersect-branches
     (lambda (s t)
       (let*-branch (((p m sl sr) s) ((q n tl tr) t))
         (cond ((branching-bit-higher? m n)
                (and (match-prefix? q p m)
                     (if (zero-bit? q m)
                         (intersect sl t)
                         (intersect sr t))))
               ((branching-bit-higher? n m)
                (and (match-prefix? p q n)
                     (if (zero-bit? p n)
                         (intersect s tl)
                         (intersect s tr))))
               ((fx=? p q)
                (smart-branch p m (intersect sl tl) (intersect sr tr)))
               (else #f))))))
    (intersect trie1 trie2)))
