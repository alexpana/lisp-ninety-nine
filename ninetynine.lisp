;; P05 Reverse a list
(defun reverse-list (l)
  (if (= (length l) 1)
      l
    (append (reverse-list (rest l)) (list (first l)))))

(assert (equal (reverse-list '(1 2 3)) '(3 2 1)))
(assert (equal (reverse-list '(1)) '(1)))
(assert (equal (reverse-list '((1) (2 3) ((4)))) '(((4)) (2 3) (1))))

;; P06 Find out whether a list is a palindrome
(defun is-palindrome (l)
  (equal l (reverse l)))

(assert (is-palindrome '(1 2 3 2 1)))
(assert (is-palindrome '(1)))
(assert (is-palindrome '(cuvant si alt si cuvant)))
(assert (not (is-palindrome '(2 3 5 8 13))))

;; P07 Flatten a nested list structure
(defun flatten (l)
  (if (= (length l) 0)
      '()
    (if (atom (car l))
        (cons (car l) (flatten (cdr l)))
      (append (flatten (car l)) (flatten (cdr l))))))

(assert (equal (flatten '(1 (2 3))) '(1 2 3)))
(assert (equal (flatten '()) '()))
(assert (equal (flatten '(((1 2)))) '(1 2)))

;; P08 Eliminate consecutive duplicates of list elements
(defun eliminate-dup (l &optional v)
  (if (= (length l) 0)
      l
    (if (equal (car l) v)
        (eliminate-dup (cdr l) v)
      (cons (car l) (eliminate-dup (cdr l) (car l))))))

(assert (equal 
          (eliminate-dup '(1 1 1 1 2 2 2 1 1 3 3 3 3 3 3 4 5 6 5 5))
          '(1 2 1 3 4 5 6 5)))

;; P09 Pack consecutive duplicates of list elements into sublists
(defun pack-consecutive (l &optional v)
  (cond
    ((null l) (list v))
    ((null v) (pack-consecutive (cdr l) (list (car l))))
    ((equal (car v) (car l)) (pack-consecutive (cdr l) (push (car l) v)))
    (T (cons v (pack-consecutive (cdr l) (list (car l)))))))

(assert (equal 
          (pack-consecutive '(1 1 1 2 2 2 2 3 3 4 5))
          '((1 1 1) (2 2 2 2) (3 3) (4) (5))))
(assert (equal
          (pack-consecutive '(1 2 2 2 2 3 3 3 3 4 4 4 4))
          '((1) (2 2 2 2) (3 3 3 3) (4 4 4 4))))

;; P10 Run-length encoding of a list
(defun rle (l &optional v)
  (cond
    ;; if the list is null, return a list containing v
    ((null l) (list (list (length v) (car v))))
    ;; if v is null, create a new v and call rle recursively
    ((null v) (rle (cdr l) (list (car l))))
    ;; if the first element in l equals the elements in v ...
    ((equal (car v) (car l)) (rle (cdr l) (push (car l) v)))
    ;; return the list formed by v and the recursive call to rle with new v
    (T (cons (list (length v) (car v)) (rle (cdr l) (list (car l)))))))

(assert (equal 
          (rle '(a a a a b c c a a a d e e e e))
          '((4 a) (1 b) (2 c) (3 a) (1 d) (4 e))))

;; P11 Modified run-length encoding (1 B) -> B
(defun modified-rle (l) (mapcar (lambda (x) (if (= (car x) 1) (car (cdr x)) x)) (rle l)))

(assert (equal 
          (modified-rle '(a a a a b c c a a a d e e e e))
          '((4 a) b (2 c) (3 a) d (4 e))))

;; P12 Decode a run-length encoded list
(defun decode-rle (l)
  (mapcan (lambda (x) 
            (if (atom x) 
                (list x) 
              (make-list (first x) :initial-element (second x)))) l))

(assert (equal 
          (decode-rle '((4 a) (2 b) d (3 c)))
          '(a a a a b b d c c c)))

;; P14 Duplicate the elements of a list
(defun dupe (l)
  (mapcan (lambda (x) (list x x)) l))

(assert (equal (dupe '(1 2 3)) '(1 1 2 2 3 3)))
(assert (equal (dupe '((1 2) 3)) '((1 2) (1 2) 3 3)))

;; P15 Replicate the elements of a list a given number of times
(defun repl (l n)
  (mapcan (lambda (x) (make-list n :initial-element x)) l))

(assert (equal (repl '(a b c) 3) '(a a a b b b c c c)))
(assert (equal (repl '((a b)) 3) '((a b) (a b) (a b))))

;; P16 Drop every N'th element from a list
(defun dropnth (l n)
  (if (>= (length l) n)
      (append (subseq l 0 (- n 1)) (dropnth (subseq l n) n))
    l))

(assert (equal (dropnth '(1 3 5 7 9) 2) '(1 5 9)))
(assert (equal (dropnth '(1 2 3 4 5) 1) '()))
;(assert (equal (dropnth '(1) 0) '(1)))

;; P17 Split a list into two parts; the length of the first one is given
(defun split (l n)
  (if (> (length l) n)
      (list (subseq l 0 n) (subseq l n))
    l))

(assert (equal (split '(1 2 3 4 5) 2) '((1 2) (3 4 5))))
(assert (equal (split '(1 2 3) 4) '(1 2 3)))

;; P18 Extract a slice from a list from I to K ( inside limits )
(defun slice (l from to)
  (append (subseq l 0 from) (subseq l (+ to 1))))

(assert (equal (slice '(1 2 3 4 5 6) 2 4) '(1 2 6)))

;; P19 Rotate a list N places to the left
(defun rotate (l n)
  (cond
	((= n 0) l)
	((< n 0) (rotate l (+ n (length l))))
	((> n 0) (rotate (append (rest l) (list (car l))) (- n 1)))))

(assert (equal (rotate '(1 2 3 4 5) 3) '(4 5 1 2 3)))
(assert (equal (rotate '(a b c d e f g h) -2) '(g h a b c d e f)))

;; P20 Remove the K'th element from a list
(defun remove-kth (l k)
  (append (subseq l 0 k) (subseq l (+ k 1) )))

(assert (equal (remove-kth '(a b c d) 2) '(a b d)))

;; P21 Insert an element at a given position into a list
(defun insert-at (x l n)
  (append (subseq l 0 n) (list x) (subseq l n)))

(assert (equal (insert-at '3 '(0 1 2 4 5) 3) '(0 1 2 3 4 5)))

;; P22 Create a list containing all integers within a given range
(defun range (a b)
  (let ((result nil)) 
	(dotimes (i (+ (- b a) 1)) (push (+ i a) result))
	(nreverse result)))

(assert (equal (range 3 5) '(3 4 5)))
(assert (equal (range -3 3) '(-3 -2 -1 0 1 2 3)))

;; P23 Extract a given number of randomly selected elements from a list
(setf *random-state* (make-random-state t)) ;sets the global random state
(defun extract (l n)
  (if (= n 0) 
	  l
	(extract (remove-kth l (random (length l))) (- n 1))))

(assert (= (length (extract '(1 2 3 4 5) 2)) 3))

;; P24 Draw N different random numbers from the set 1..M
(defun lotto-select (n m)
  (let ((u nil))
	(dotimes (i n)
	  (push (random m) u))
	u))

;; P25 Generate a random permutation of the elements of a list

