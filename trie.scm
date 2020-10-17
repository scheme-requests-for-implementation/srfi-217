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

(define (branching-bit p0 m0 p1 m1)
  (if (fxnegative? (fxxor p0 p1))
      fx-least        ; different signs
      (highest-bit-mask (fxxor p0 p1) (fxmax 1 (fx* 2 (fxmax m0 m1))))))

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
              (let ((p (branch-prefix t))
                    (m (branch-branching-bit t)))
                (if (match-prefix? key p m)
                    (if (zero-bit? key m)
                        (branch p m (ins (branch-left t)) (branch-right t))
                        (branch p m (branch-left t) (ins (branch-right t))))
                    (trie-join key 0 key p m t))))))))
    (ins trie)))

(define (trie-join p0 m0 t0 p1 m1 t1)
  (let ((m (branching-bit p0 m0 p1 m1)))
    (if (zero-bit? p0 m)
        (branch (mask p0 m) m t0 t1)
        (branch (mask p0 m) m t1 t0))))

(define (trie-contains? trie key)
  (and trie
       (if (integer? trie)
           (fx=? key trie)
           (let ((m (branch-branching-bit trie)))
             (and (match-prefix? key (branch-prefix trie) m)
                  (if (zero-bit? key m)
                      (trie-contains? (branch-left trie) key)
                      (trie-contains? (branch-right trie) key)))))))

(define (trie-merge trie0 trie1)
  (letrec
    ((merge
      (lambda (s t)
        (cond ((not s) t)
              ((not t) s)
              ((integer? s) (trie-insert t s))
              ((integer? t) (trie-insert s t))
              (else (merge-branches s t)))))
     (merge-branches
      (lambda (s t)
        (let*-branch (((p m s0 s1) s)
                      ((q n t0 t1) t))
          (cond ((and (fx=? m n) (fx=? p q))
                 ;; the prefixes match, so merge the subtries
                 (branch p m (merge s0 t0) (merge s1 t1)))
                ((and (fx>? m n) (match-prefix? q p m))
                 ;; p is a prefix of q, so merge t with a subtrie of s.
                 (if (zero-bit? q m)
                     (branch p m (merge s0 t) s1)
                     (branch p m s0 (merge s1 t))))
                ((and (fx<? m n) (match-prefix? p q n))
                 ;; q is a prefix of p, so merge s with a subtrie of t.
                 (if (zero-bit? p n)
                     (branch q n (merge s t0) t1)
                     (branch q n t0 (merge s t1))))
                (else    ; the prefixes disagree
                 (trie-join p m s q n t)))))))
    (merge trie0 trie1)))

;; Construct a branch only if the subtrees s and t are non-empty.
(define (smart-branch p m s t)
  (cond ((not s) t)
        ((not t) s)
        (else (branch p m s t))))

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

(define (trie=? s t)
  (cond ((not (or s t)) #t)
        ((and (integer? s) (integer? t)) (fx=? s t))
        ((and (branch? s) (branch? t))
         (let*-branch (((p m s0 s1) s) ((q n t0 t1) t))
           (and (fx=? m n) (fx=? p q) (trie=? s0 t0) (trie=? s1 t1))))
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
         (cond ((fx>? m n) 'greater)
               ((fx<? m n)
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

(define (trie-disjoint? trie0 trie1)
  (letrec
   ((disjoint?
     (lambda (s t)
       (or (not s)
           (not t)
           (cond ((integer? s)
                  (if (integer? t)
                      (not (fx=? s t))
                      (not (trie-contains? t s))))
                 ((integer? t) (not (trie-contains? t s)))
                 (else (branches-disjoint? s t))))))
    (branches-disjoint?
     (lambda (s t)
       (let*-branch (((p m s0 s1) s)
                    ((q n t0 t1) t))
         (cond ((and (fx=? m n) (fx=? p q))
                (and (disjoint? s0 t0) (disjoint? s1 t1)))
               ((and (fx>? m n) (match-prefix? q p m))
                (if (zero-bit? q m)
                    (disjoint? s0 t)
                    (disjoint? s1 t)))
               ((and (fx<? m n) (match-prefix? p q n))
                (if (zero-bit? p n)
                    (disjoint? s t0)
                    (disjoint? s t1)))
               (else #t))))))      ; the prefixes disagree
    (disjoint? trie0 trie1)))

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
