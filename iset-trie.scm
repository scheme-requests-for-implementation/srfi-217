;;; Copyright (C) 2020 Wolfgang Corcoran-Mathe
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

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

;;;; Integer sets

(define-record-type <iset>
  (raw-iset trie)
  iset?
  (trie iset-trie))

;;;; Constructors

(define (iset . args)
  (list->iset args))

(define (list->iset ns)
  (raw-iset
   (fold (lambda (n t)
           (assume (valid-integer? n))
           (trie-insert t n))
         #f
         ns)))

(define (iset-unfold stop? mapper successor seed)
  (assume (procedure? stop?))
  (assume (procedure? mapper))
  (assume (procedure? successor))
  (let lp ((trie #f) (seed seed))
    (if (stop? seed)
        (raw-iset trie)
        (let ((n (mapper seed)))
          (assume (valid-integer? n))
          (lp (trie-insert trie n) (successor seed))))))

;; TODO: Bitmap compression will enable a much more efficient version
;; of this.
(define (make-iset-range low high)
  (assume (valid-integer? low))
  (assume (valid-integer? high))
  (assume (>= high low))
  (iset-unfold (lambda (i) (= i high))
               values
               (lambda (i) (+ i 1))
               low))

;;;; Predicates

(define (iset-contains? set n)
  (assume (iset? set))
  (assume (valid-integer? n))
  (trie-contains? (iset-trie set) n))

(define (iset-empty? set)
  (assume (iset? set))
  (not (iset-trie set)))

(define (iset-disjoint? set1 set2)
  (assume (iset? set1))
  (assume (iset? set2))
  (trie-disjoint? (iset-trie set1) (iset-trie set2)))

;;;; Accessors

(define (iset-min set)
  (assume (iset? set))
  (let ((trie (iset-trie set)))
    (if (branch? trie)
        (%trie-find-leftmost
         (if (negative? (branch-branching-bit trie))
             (branch-right trie)
             (branch-left trie)))
        trie)))  ; #f or leaf

(define (iset-max set)
  (assume (iset? set))
  (let ((trie (iset-trie set)))
    (if (branch? trie)
        (%trie-find-rightmost
         (if (negative? (branch-branching-bit trie))
             (branch-left trie)
             (branch-right trie)))
        trie)))  ; #f or leaf

;;;; Updaters

;; FIXME: Not in the pre-SRFI, but should be added.
(define (iset-adjoin set n)
  (assume (iset? set))
  (assume (valid-integer? n))
  (raw-iset (trie-insert (iset-trie set) n)))

(define (iset-adjoin! set n) (iset-adjoin set n))

(define (iset-delete set n)
  (assume (iset? set))
  (assume (valid-integer? n))
  (raw-iset (trie-remove (lambda (m) (fx=? n m)) (iset-trie set))))

(define (iset-delete! set n) (iset-delete set n))

;; FIXME: Not in the pre-SRFI, but should be added.
;; Implement this in terms of set difference?
(define (iset-delete-all set ns)
  (assume (iset? set))
  (assume (or (pair? ns) (null? ns)))
  (raw-iset (trie-remove (lambda (m) (member m ns fx=?))
                         (iset-trie set))))

(define (iset-delete-all! set ns)
  (iset-delete-all set ns))

;; The search/rebuild process is complicated by the need to handle
;; a negative branching-bit (which can only occur at the root of the
;; trie) differently.
(define (iset-delete-min set)
  (assume (iset? set))
  (letrec
   ((update
     (lambda (t)
       (cond ((not t) (values #f #f))
             ((integer? t) (values t #f))
             (else (update-branch t)))))
    (update-branch
     (lambda (t)
       (let*-branch (((p m l r) t))
         (if (negative? m)
             (let-values (((n r*) (update r)))   ; root node only
               (values n (branch p m l r*)))
             (let-values (((n l*) (update l)))
               (values n (branch p m l* r))))))))
    (let*-values (((trie) (iset-trie set))
                  ((least trie*) (update trie)))
      (values least (raw-iset trie*)))))

(define (iset-delete-max set)
  (assume (iset? set))
  (letrec
   ((update
     (lambda (t)
       (cond ((not t) (values #f #f))
             ((integer? t) (values t #f))
             (else (update-branch t)))))
    (update-branch
     (lambda (t)
       (let*-branch (((p m l r) t))
         (if (negative? m)
             (let-values (((n l*) (update l)))   ; root node only
               (values n (branch p m l* r)))
             (let-values (((n r*) (update r)))
               (values n (branch p m l r*))))))))
    (let*-values (((trie) (iset-trie set))
                  ((least trie*) (update trie)))
      (values least (raw-iset trie*)))))


(define (iset-delete-min! set) (iset-delete-min set))
(define (iset-delete-max! set) (iset-delete-max set))

;;;; The whole iset

(define (iset-size set)
  (assume (iset? set))
  (let lp ((acc 0) (t (iset-trie set)))
    (cond ((not t) acc)
          ((integer? t) (+ acc 1))
          (else
           (lp (lp acc (branch-left t)) (branch-right t))))))

(define (iset-count pred set)
  (assume (procedure? pred))
  (iset-fold (lambda (n acc)
               (if (pred n) (+ 1 acc) acc))
             0
             set))

(define (iset-any? pred set)
  (assume (procedure? pred))
  (call-with-current-continuation
   (lambda (return)
     (iset-fold (lambda (n _)
                  (and (pred n) (return #t)))
                #f
                set))))

(define (iset-every? pred set)
  (assume (procedure? pred))
  (call-with-current-continuation
   (lambda (return)
     (iset-fold (lambda (n _)
                  (or (pred n) (return #f)))
                #f
                set))))

;;;; Mapping and folding

(define (iset-map proc set)
  (assume (procedure? proc))
  (raw-iset
   (iset-fold (lambda (n t)
                (let ((n* (proc n)))
                  (assume (valid-integer? n*))
                  (trie-insert t (proc n))))
              #f
              set)))

(define (unspecified)
  (if #f #f))

(define (iset-for-each proc set)
  (assume (procedure? proc))
  (iset-fold (lambda (n _)
               (proc n)
               (unspecified))
             (unspecified)
             set))

(define (iset-fold proc nil set)
  (assume (procedure? proc))
  (assume (iset? set))
  (letrec
   ((cata
     (lambda (b t)
       (cond ((not t) b)
             ((integer? t) (proc t b))
             (else
              (cata (cata b (branch-left t)) (branch-right t)))))))
    (let ((trie (iset-trie set)))
      (if (branch? trie)
          (if (negative? (branch-branching-bit trie))
              (cata (cata nil (branch-left trie)) (branch-right trie))
              (cata (cata nil (branch-right trie)) (branch-left trie)))
          (cata nil trie)))))

(define (iset-filter pred set)
  (assume (procedure? pred))
  (assume (iset? set))
  (raw-iset (trie-filter pred (iset-trie set))))

(define (iset-remove pred set)
  (assume (procedure? pred))
  (assume (iset? set))
  (raw-iset (trie-filter (lambda (n) (not (pred n))) (iset-trie set))))

;;;; Copying and conversion

(define (iset-copy set)
  (assume (iset? set))
  (letrec
   ((copy-trie
     (lambda (t)
       (and t
            (if (integer? t)
                t
                (branch (branch-prefix t)
                        (branch-branching-bit t)
                        (copy-trie (branch-left t))
                        (copy-trie (branch-right t))))))))
    (raw-iset (copy-trie (iset-trie set)))))

(define (iset->list set)
  (iset-fold cons '() set))

;;;; Comparison

(define (iset=? set1 set2)
  (assume (iset? set1))
  (assume (iset? set2))
  (letrec
   ((trie=?
     (lambda (s t)
       (cond ((not (or s t)) #t)
             ((and (integer? s) (integer? t)) (fx=? s t))
             ((and (branch? s) (branch? t))
              (let*-branch (((p m s0 s1) s) ((q n t0 t1) t))
                (and (fx=? m n) (fx=? p q) (trie=? s0 t0) (trie=? s1 t1))))
             (else #f)))))
    (or (eqv? set1 set2)    ; quick check
        (trie=? (iset-trie set1) (iset-trie set2)))))

(define (iset<? set1 set2)
  (assume (iset? set1))
  (assume (iset? set2))
  (error "not implemented"))

(define (iset>? set1 set2)
  (assume (iset? set1))
  (assume (iset? set2))
  (error "not implemented"))

(define (iset<=? set1 set2)
  (assume (iset? set1))
  (assume (iset? set2))
  (error "not implemented"))

(define (iset>=? set1 set2)
  (assume (iset? set1))
  (assume (iset? set2))
  (error "not implemented"))

;;;; Set theory operations

(define (iset-union set1 set2)
  (assume (iset? set1))
  (assume (iset? set2))
  (raw-iset (trie-merge (iset-trie set1) (iset-trie set2))))

(define (iset-union! set1 set2) (iset-union set1 set2))
