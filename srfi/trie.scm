;;; A trie is represented by #f (the empty trie), a leaf, or a branch.
;;;
;;; Throughout this code, the empty trie (#f) is always returned
;;; as an explicit value, not, e.g. as the default value of an
;;; (and ...) expression, to clarify its use as a trie value.

(define-record-type <leaf>
  (raw-leaf prefix bitmap)
  leaf?
  (prefix leaf-prefix)
  (bitmap leaf-bitmap))

;; Construct a leaf only if the bitmap is non-zero.
(define (leaf prefix bitmap)
  (if (fxpositive? bitmap)
      (raw-leaf prefix bitmap)
      #f))

;; Gives the maximum number of integers storable in a single leaf.
(define leaf-bitmap-size (expt 2 (exact (floor (log fx-width 2)))))

(define suffix-mask (- leaf-bitmap-size 1))
(define prefix-mask (fxnot suffix-mask))

;; Shorthand for extracting leaf elements.
(define-syntax let*-leaf
  (syntax-rules ()
    ((_ () e1 e2 ...) (begin e1 e2 ...))
    ((_ (((p b) expr) . binds) . body)
     (let ((lf expr))
       (let ((p (leaf-prefix lf)) (b (leaf-bitmap lf)))
         (let*-leaf binds . body))))))

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

;; FIXME: To improve.
(define (highest-set-bit k)
  (fx- (fxlength (highest-bit-mask k 1)) 1))

(define (zero-bit? k m)
  (fxzero? (fxand k m)))

(define (isuffix k)
  (fxand k suffix-mask))

(define (iprefix k)
  (fxand k prefix-mask))

(define (ibitmap k)
  (fxarithmetic-shift 1 (isuffix k)))

(define (trie-insert-parts trie prefix bitmap)
  (letrec
   ((ins
     (lambda (t)
       (cond ((not t) (raw-leaf prefix bitmap))
             ((leaf? t)
              (let*-leaf (((p b) t))
                (if (fx=? prefix p)
                    (raw-leaf prefix (fxior b bitmap))
                    (trie-join prefix 0 (raw-leaf prefix bitmap) p 0 t))))
             (else
              (let*-branch (((p m l r) t))
                (if (match-prefix? prefix p m)
                    (if (zero-bit? prefix m)
                        (branch p m (ins l) r)
                        (branch p m l (ins r)))
                    (trie-join prefix 0 (raw-leaf prefix bitmap) p m t))))))))
    (ins trie)))

(define (trie-insert trie key)
  (trie-insert-parts trie (iprefix key) (ibitmap key)))

(define (trie-join prefix1 mask1 trie1 prefix2 mask2 trie2)
  (let ((m (branching-bit prefix1 mask1 prefix2 mask2)))
    (if (zero-bit? prefix1 m)
        (branch (mask prefix1 m) m trie1 trie2)
        (branch (mask prefix1 m) m trie2 trie1))))

(define (trie-contains? trie key)
  (and trie
       (if (leaf? trie)
           (and (fx=? (iprefix key) (leaf-prefix trie))
                (not (fxzero? (fxand (ibitmap key) (leaf-bitmap trie)))))
           (let*-branch (((p m l r) trie))
             (and (match-prefix? key p m)
                  (if (zero-bit? key m)
                      (trie-contains? l key)
                      (trie-contains? r key)))))))

(define (branching-bit-higher? mask1 mask2)
  (if (negative? (fxxor mask1 mask2))  ; signs differ
      (negative? mask1)
      (fx>? mask1 mask2)))

;; Merge two tries.  The exact contents of the result depend on the
;; `insert-leaf' function, which is used to merge leaves into branches.
;; Taking the running time of `insert-leaf' to be constant, runs in
;; O(n+m) time.
(define (trie-merge insert-leaf trie1 trie2)
  (letrec
    ((merge
      (lambda (s t)
        (cond ((not s) t)
              ((not t) s)
              ((and (leaf? s) (leaf? t))
               (let*-leaf (((p b) s) ((q c) t))
                 (if (fx=? p q)
                     (raw-leaf p (fxior b c))
                     (trie-join p 0 s q 0 t))))
              ((leaf? s) (insert-leaf t s))
              ((leaf? t) (insert-leaf s t))
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

(define (trie-union trie1 trie2)
  (trie-merge (lambda (s t) (%insert-leaf/proc fxior s t))
              trie1
              trie2))

(define (trie-xor trie1 trie2)
  (trie-merge (lambda (s t) (%insert-leaf/proc fxxor s t))
              trie1
              trie2))

;; Insert the elements of `lf' into `trie', combining bitmaps with
;; the binary bitwise operation `fxcombine'.
(define (%insert-leaf/proc fxcombine trie lf)
  (let*-leaf (((p bm) lf))
    (letrec
     ((ins
       (lambda (t)
         (cond ((not t) lf)  ; a whole new leaf
               ((leaf? t)
                (let*-leaf (((q bm*) t))
                  (if (fx=? p q)
                      (raw-leaf p (fxcombine bm bm*))
                      (trie-join p 0 lf q 0 t))))
               (else         ; branch
                (let*-branch (((q m l r) t))
                  (if (match-prefix? p q m)
                      (if (zero-bit? p m)
                          (branch p m (ins l) r)
                          (branch p m l (ins r)))
                      (trie-join p 0 lf q 0 t))))))))
      (ins trie))))

(define (copy-trie trie)
  (cond ((not trie) #f)
        ((leaf? trie) trie)
        (else
         (branch (branch-prefix trie)
                 (branch-branching-bit trie)
                 (copy-trie (branch-left trie))
                 (copy-trie (branch-right trie))))))

;;;; Iteration

;; Left branches are processed before right.
(define (trie-fold proc nil trie)
  (letrec
   ((cata
     (lambda (b t)
       (cond ((not t) b)
             ((leaf? t)
              (fold-left-bits (leaf-prefix t) proc b (leaf-bitmap t)))
             (else
              (cata (cata b (branch-left t)) (branch-right t)))))))
    (cata nil trie)))

(define (fold-left-bits prefix proc nil bitmap)
  (let loop ((bm bitmap) (acc nil))
    (if (fxzero? bm)
        acc
        (let* ((mask (lowest-set-bit bm))
               (bi (fxfirst-set-bit mask)))
          (loop (fxxor bm mask) (proc acc (fx+ prefix bi)))))))

(define (bitmap-partition pred prefix bitmap)
  (let loop ((i 0) (in 0) (out 0))
    (cond ((fx=? i leaf-bitmap-size) (values in out))
          ((fxbit-set? i bitmap)
           (let ((bit (fxarithmetic-shift 1 i)))
             (if (pred (fx+ prefix i))
                 (loop (fx+ i 1) (fxior in bit) out)
                 (loop (fx+ i 1) in (fxior out bit)))))
          (else (loop (fx+ i 1) in out)))))

(define (trie-partition pred trie)
  (letrec
   ((part
     (lambda (t)
       (cond ((not t) (values #f #f))
             ((leaf? t)
              (let*-leaf (((p bm) t))
                (let-values (((in out) (bitmap-partition pred p bm)))
                  (values (leaf p in) (leaf p out)))))
             (else
              (let-values (((p) (branch-prefix t))
                           ((m) (branch-branching-bit t))
                           ((il ol) (part (branch-left t)))
                           ((ir or) (part (branch-right t))))
                (values (smart-branch p m il ir)
                        (smart-branch p m ol or))))))))
    (part trie)))

(define (bitmap-filter pred prefix bitmap)
  (let loop ((i 0) (res 0))
    (cond ((fx=? i leaf-bitmap-size) res)
          ((and (fxbit-set? i bitmap) (pred (fx+ prefix i)))
           (loop (fx+ i 1) (fxior res (fxarithmetic-shift 1 i))))
          (else (loop (fx+ i 1) res)))))

(define (trie-filter pred trie)
  (cond ((not trie) #f)
        ((leaf? trie)
         (let*-leaf (((p bm) trie))
           (leaf p (bitmap-filter pred p bm))))
        (else
         (smart-branch (branch-prefix trie)
                       (branch-branching-bit trie)
                       (trie-filter pred (branch-left trie))
                       (trie-filter pred (branch-right trie))))))

(define (trie-remove pred trie)
  (cond ((not trie) #f)
        ((leaf? trie)
         (let*-leaf (((p bm) (leaf-prefix trie)))
           (leaf p
                 (bitmap-filter (lambda (x) (not (pred x)))
                                p
                                bm))))
        (else
         (smart-branch (branch-prefix trie)
                       (branch-branching-bit trie)
                       (trie-remove pred (branch-left trie))
                       (trie-remove pred (branch-right trie))))))

(define (%trie-find-least trie)
  (and trie
       (if (leaf? trie)
           (fx+ (leaf-prefix trie) (fxfirst-set-bit (leaf-bitmap trie)))
           (%trie-find-least (branch-left trie)))))

(define (%trie-find-greatest trie)
  (and trie
       (if (leaf? trie)
           (fx+ (leaf-prefix trie) (highest-set-bit (leaf-bitmap trie)))
           (%trie-find-greatest (branch-right trie)))))

;;;; Comparisons

(define (trie=? trie1 trie2)
  (cond ((not (or trie1 trie2)) #t)
        ((and (leaf? trie1) (leaf? trie2))
         (and (fx=? (leaf-prefix trie1) (leaf-prefix trie2))
              (fx=? (leaf-bitmap trie1) (leaf-bitmap trie2))))
        ((and (branch? trie1) (branch? trie2))
         (let*-branch (((p m l1 r1) trie1) ((q n l2 r2) trie2))
           (and (fx=? m n) (fx=? p q) (trie=? l1 l2) (trie=? r1 r2))))
        (else #f)))

(define (subset-compare-leaves l1 l2)
  (let*-leaf (((p b) l1) ((q c) l2))
    (if (fx=? p q)
        (if (fx=? b c)
            'equal
            (if (fxzero? (fxand b (fxnot c)))
                'less
                'greater))
        'greater)))  ; disjoint

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
             ((and (leaf? s) (leaf? t)) (subset-compare-leaves s t))
             ((leaf? s)             ; leaf / branch
              (let*-leaf (((p _) s))
                (let*-branch (((q m l r) t))
                  (if (match-prefix? p q m)
                      (case (compare s (if (zero-bit? p m) l r))
                        ((greater) 'greater)
                        (else 'less))
                      'greater))))       ; disjoint
             ((leaf? t) 'greater)        ; branch / leaf
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
           (cond ((and (leaf? s) (leaf? t)) (disjoint/leaf? s t))
                 ((leaf? s) (disjoint/leaf? s t))
                 ((leaf? t) (disjoint/leaf? t s))
                 (else (branches-disjoint? s t))))))
    (disjoint/leaf?
     (lambda (lf t)
       (let*-leaf (((p bm) lf))
         (let lp ((t t))
           (if (leaf? t)
               (if (fx=? p (leaf-prefix t))
                   (fxzero? (fxand bm (leaf-bitmap t)))
                   #t)
               (let*-branch (((q n l r) t))
                 (if (match-prefix? p q n)
                     (if (zero-bit? p n) (lp l) (lp r))
                     #t)))))))
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

(define (bitmap-delete bitmap prefix key)
  (fxxor bitmap
         (fxarithmetic-shift 1 (fx- key prefix))))

(define (trie-delete trie key)
  (letrec
   ((update
     (lambda (t)
       (cond ((not t) #f)
             ((leaf? t)
              (let*-leaf (((p bm) t))
                (leaf p (bitmap-delete bm p key))))
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

;; Construct a trie which forms the intersection of the two tries.
;; Runs in O(n+m) time.
(define (trie-intersection trie1 trie2)
  (letrec
   ((intersect
     (lambda (s t)
       (cond ((or (not s) (not t)) #f)
             ((leaf? s) (intersect/leaf s t))
             ((leaf? t) (intersect/leaf t s))
             (else (intersect-branches s t)))))
    (intersect/leaf
     (lambda (l t)
       (let*-leaf (((p bm) l))
         (let lp ((t t))
           (cond ((not t) #f)
                 ((leaf? t)
                  (if (fx=? p (leaf-prefix t))
                      (leaf p (fxand bm (leaf-bitmap t)))
                      #f))          ; disjoint
                 (else              ; branch
                  (let*-branch (((q m l r) t))
                    (if (match-prefix? p q m)
                        (if (zero-bit? p m) (lp l) (lp r))
                        #f))))))))  ; disjoint
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

;; Construct a trie containing the elements of trie1 not found in trie2.
;; Runs in O(n+m) time.
(define (trie-difference trie1 trie2)
  (letrec
   ((difference
     (lambda (s t)
       (cond ((not s) #f)
             ((not t) s)
             ((leaf? s) (diff/leaf s t))
             ((leaf? t)
              (%trie-delete-bitmap s (leaf-prefix t) (leaf-bitmap t)))
             (else (branch-difference s t)))))
    (diff/leaf
     (lambda (lf t)
       (let*-leaf (((p bm) lf))
         (let lp ((t t))
           (cond ((not t) lf)
                 ((leaf? t)
                  (let*-leaf (((q c) t))
                    (if (fx=? p q)
                        (leaf p (fxand bm (fxnot c)))
                        lf))) ; disjoint
                 (else        ; branch
                  (let*-branch (((q m l r) t))
                    (if (match-prefix? p q m)
                        (if (zero-bit? p m) (lp l) (lp r))
                        lf))))))))
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

;; Delete all values described by `bitmap' from `trie'.
(define (%trie-delete-bitmap trie prefix bitmap)
  (cond ((not trie) #f)
        ((leaf? trie)
         (if (fx=? prefix (leaf-prefix trie))
             (leaf prefix (fxand (leaf-bitmap trie) (fxnot bitmap)))
             trie))  ; disjoint
        (else        ; branch
         (let*-branch (((p m l r) trie))
           (if (match-prefix? prefix p m)
               (if (zero-bit? prefix m)
                   (smart-branch p m (%trie-delete-bitmap l prefix bitmap) r)
                   (smart-branch p m l (%trie-delete-bitmap r prefix bitmap)))
               trie)))))

;; Return a trie containing all the elements of `trie' which are
;; less than k, if `inclusive' is false, or less than or equal to
;; k if `inclusive' is true.
;; Runs in O(min(n, W)) time.
(define (subtrie< trie k inclusive)
  (letrec
    ((split
      (lambda (t)
        (cond ((not t) #f)
              ((leaf? t)
               (let*-leaf (((p bm) t))
                 (leaf p (bitmap-split< k inclusive p bm))))
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

;; Return a bitmap containing all elements in `bitmap' that are
;; less than/less than or equal to k.
(define (bitmap-split< k inclusive prefix bitmap)
  (let ((kp (iprefix k)) (kb (ibitmap k)))
    (cond ((fx>? kp prefix) bitmap)
          ((fx=? kp prefix)
           (fxand bitmap
                  (fx- (if inclusive
                           (fxarithmetic-shift kb 1)
                           kb)
                       1)))
          (else 0))))

;; Return a trie containing all the elements of `trie' which are
;; greater than k, if `inclusive' is false, or greater than or equal
;; to k if `inclusive' is true.
;; Runs in O(min(n, W)) time.
(define (subtrie> trie k inclusive)
  (letrec
   ((split
     (lambda (t)
       (cond ((not t) #f)
             ((leaf? t)
              (let*-leaf (((p bm) t))
                (leaf p (bitmap-split> k inclusive p bm))))
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

;; Return a bitmap containing all elements in `bitmap' that are
;; greater than/greater than or equal to `k'.
(define (bitmap-split> k inclusive prefix bitmap)
  (let ((kp (iprefix k)) (kb (ibitmap k)))
    (cond ((fx<? kp prefix) bitmap)
          ((fx=? kp prefix)
           (fxand bitmap
                  (fxneg (if inclusive
                             kb
                             (fxarithmetic-shift kb 1)))))
          (else 0))))

;; Return a trie containing all the elements of `trie' which are
;; greater than/greater than or equal to a and less than/less than
;; or equal to b, depending on the truth values of
;; low-/high-inclusive.
(define (subtrie-interval trie a b low-inclusive high-inclusive)
  (letrec
   ((interval
     (lambda (t)
       (cond ((not t) #f)
             ((leaf? t)
              (let*-leaf (((p bm) t))
                (leaf p (bitmap-interval p
                                         bm
                                         a
                                         b
                                         low-inclusive
                                         high-inclusive))))
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

;; Return a bitmap containing the elements of bitmap that are within
;; the interval defined by a, b.
(define (bitmap-interval prefix bitmap low high low-inclusive high-inclusive)
  (let ((lp (iprefix low))
        (lb (ibitmap low))
        (hp (iprefix high))
        (hb (ibitmap high)))
    (let ((low-mask (fxneg (if low-inclusive    ; mask everything above `low'
                               lb
                               (fxarithmetic-shift lb 1))))
          (high-mask (fx- (if high-inclusive    ; mask everything below `high'
                              (fxarithmetic-shift hb 1)
                              hb)
                          1)))
      (cond ((fx<? prefix hp)
             (cond ((fx<? prefix lp) 0)
                   ((fx>? prefix lp) bitmap)
                   (else (fxand low-mask bitmap))))
            ((fx>? prefix hp) 0)
            (else (fxand (fxand low-mask high-mask) bitmap))))))

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
                ((leaf? t)
                 (let*-leaf (((p bm) t))
                   (if (and (fx=? (iprefix key) p)
                            (not (fxzero? (fxand (ibitmap key) bm))))
                       (success key
                                (lambda (elt obj)  ; discard the new trie
                                  (if (eqv? elt key)
                                      (values key obj)
                                      (abort (trie-insert
                                              (trie-delete trie key)
                                              elt)
                                              obj)))
                                (lambda (obj)
                                  (values (leaf p (bitmap-delete bm p key))
                                          obj)))
                       (failure (lambda (obj)                   ; insert
                                  (let ((lf (raw-leaf (iprefix key)
                                                      (ibitmap key))))
                                    (values
                                     (trie-join (iprefix key) 0 lf p 0 t)
                                     obj)))
                                (lambda (obj)                   ; ignore
                                  (values #f obj))))))
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
