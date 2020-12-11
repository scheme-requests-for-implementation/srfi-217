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

(define (trie-union s t)
  (trie-merge trie-insert s t))

(define (copy-trie trie)
  (and trie
       (if (integer? trie)
           trie
           (branch (branch-prefix trie)
                   (branch-branching-bit trie)
                   (copy-trie (branch-left trie))
                   (copy-trie (branch-right trie))))))

(define (trie-partition pred trie)
  (letrec
   ((part
     (lambda (t)
       (cond ((not t) (values #f #f))
             ((integer? t)
              (if (pred t)
                  (values t #f)
                  (values #f t)))
             (else
              (let-values (((p) (branch-prefix t))
                           ((m) (branch-branching-bit t))
                           ((il ol) (part (branch-left t)))
                           ((ir or) (part (branch-right t))))
                (values (smart-branch p m il ir)
                        (smart-branch p m ol or))))))))
    (part trie)))

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

(define (trie-difference trie1 trie2)
  (letrec
   ((difference
     (lambda (s t)
       (cond ((not s) #f)
             ((not t) s)
             ((integer? s) (diff-int s t))
             ((integer? t) (trie-delete s t))
             (else (branch-difference s t)))))
    (diff-int
     (lambda (n t)
       (cond ((not t) n)
             ((integer? t) (if (fx=? t n) #f n))
             (else
              (let*-branch (((p m l r) t))
                (if (match-prefix? n p m)
                    (if (zero-bit? n m)
                        (diff-int n l)
                        (diff-int n r))
                    n))))))
    (branch-difference
     (lambda (s t)
       (let*-branch (((p m sl sr) s) ((q n tl tr) t))
         (cond ((and (fx=? m n) (fx=? p q))
                (smart-branch p m (difference sl tl) (difference sr tr)))
               ((and (branching-bit-higher? m n) (match-prefix? q p m))
                (if (zero-bit? q m)
                    (smart-branch p m (difference sl t) sr)
                    (smart-branch p m sl (difference sr t))))
               ((and (branching-bit-higher? n m) (match-prefix? p q n))
                (if (zero-bit? p n)
                    (difference s tl)
                    (difference s tr)))
               (else s))))))
    (difference trie1 trie2)))

;; Return a trie containing all the elements of `trie' which are
;; less than k, if `inclusive' is false, or less than or equal to
;; k if `inclusive' is true.
(define (subtrie< trie k inclusive)
  (letrec
    ((split
      (lambda (t)
        (cond ((not t) #f)
	      ((integer? t)
	       (cond ((fx<? t k) t)
		     ((and (fx=? t k) inclusive) t)
		     (else #f)))
              (else
               (let*-branch (((p m l r) t))
                 (if (match-prefix? k p m)
                     (if (zero-bit? k m)
                         (split l)
                         (trie-union l (split r)))
                     (and (fx<? p k) t))))))))
    (if (and (branch? trie) (fxnegative? (branch-branching-bit trie)))
        (if (fxnegative? k)
            (split (branch-right trie))
            (trie-union (split (branch-left trie)) (branch-right trie)))
        (split trie))))

;; Return a trie containing all the elements of `trie' which are
;; greater than k, if `inclusive' is false, or greater than or equal
;; to k if `inclusive' is true.
(define (subtrie> trie k inclusive)
  (letrec
   ((split
     (lambda (t)
       (cond ((not t) #f)
	     ((integer? t)
	      (cond ((fx>? t k) t)
		    ((and (fx=? t k) inclusive) t)
		    (else #f)))
	     (else
	      (let*-branch (((p m l r) t))
		(if (match-prefix? k p m)
		    (if (zero-bit? k m)
			(trie-union (split l) r)
			(split r))
		    (and (fx>? p k) t))))))))
    (if (and (branch? trie) (fxnegative? (branch-branching-bit trie)))
	(if (fxnegative? k)
	    (trie-union (split (branch-right trie)) (branch-left trie))
	    (split (branch-left trie)))
	(split trie))))

;; Return a trie containing all the elements of `trie' which are
;; greater than/greater than or equal to a and less than/less than
;; or equal to b, depending on the truth values of
;; low-/high-inclusive.
(define (subtrie-interval trie a b low-inclusive high-inclusive)
  (letrec
   ((interval
     (lambda (t)
       (cond ((not t) #f)
             ((integer? t) (and ((if low-inclusive fx>=? fx>?) t a)
                                ((if high-inclusive fx<=? fx<?) t b)
                                t))
             (else (branch-interval t)))))
    (branch-interval
     (lambda (t)
       (let*-branch (((p m l r) t))
         (if (match-prefix? a p m)
             (if (zero-bit? a m)
                 (if (match-prefix? b p m)
                     (if (zero-bit? b m)
                         (interval l)  ; all x < b is in l
                         (trie-union (subtrie> l a low-inclusive)
                                     (subtrie< r b high-inclusive)))
                     ;; everything or nothing is less than b
                     (and (fx<? b p)
                          (trie-union (subtrie> l a low-inclusive) r)))
                 (interval r)) ; all x > b is in r
             ;; everything or nothing is greater than a
             (and (fx>? p a) (subtrie< t b high-inclusive)))))))
    (if (and (branch? trie) (fxnegative? (branch-branching-bit trie)))
	(cond ((and (fxnegative? a) (fxnegative? b))
	       (interval (branch-right trie)))
	      ((and (fxpositive? a) (fxpositive? b))
	       (interval (branch-left trie)))
	      ;; (a, 0) U (0, b)
	      (else (trie-union (subtrie> (branch-right trie) a low-inclusive)
	                        (subtrie< (branch-left trie) b high-inclusive))))
	(interval trie))))

;; Search trie for key, and construct a new trie using the results of
;; failure and success.
;;
;; Note that the first continuation passed to success may abort the
;; construction of the new trie and simply insert the new element
;; into the old one.  This is necessitated by the fact that the new
;; element may be anything, and thus can't always replace `key'
;; "in place".
(define (trie-search trie key failure success)
  (call-with-current-continuation
   (lambda (abort)
     (letrec
      ((search
        (lambda (t)
          (cond ((not t)
                 (failure (lambda (obj)          ; insert
                            (values key obj))
                          (lambda (obj)          ; ignore
                            (values #f obj))))
                ((integer? t)
                 (if (fx=? t key)
                     (success key
                              (lambda (elt obj)  ; discard the new trie
                                (if (eqv? elt key)
                                    (values key obj)
                                    (abort (trie-insert
                                            (trie-delete trie key)
                                            elt)
                                           obj)))
                              (lambda (obj) (values #f obj)))
                     (failure (lambda (obj)                   ; insert
                                (values (trie-join key 0 key t 0 t) obj))
                              (lambda (obj)                   ; ignore
                                (values #f obj)))))
                (else
                 (let*-branch (((p m l r) t))
                   (if (match-prefix? key p m)
                       (if (zero-bit? key m)
                           (let-values (((l* obj) (search l)))
                             (values (smart-branch p m l* r) obj))
                           (let-values (((r* obj) (search r)))
                             (values (smart-branch p m l r*) obj)))
                       (failure (lambda (obj)                 ; insert
                                  (values (trie-join key 0 key p m t) obj))
                                (lambda (obj)                 ; ignore
                                  (values t obj))))))))))
       (search trie)))))
