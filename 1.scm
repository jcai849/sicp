;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 1.2

(/ (+ 5 4
      (- 2
	 (- 3
	    (+ 6
	       (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 1.3


(define (ss x y) (+ (* x x)
		    (* y y)))

(define (ss-3 x y z) (cond ((and (> x z) (> y z)) (ss x y))
			   ((and (> x y) (> z y)) (ss x z))
			   ((and (> y x) (> z x)) (ss y z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 1.6

;;; It won't work due to the applicative-order evaluation; the else
;;; statement will be continually evaluated as well as the if's, thus
;;; creating an endless loop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; sqrt code

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
		 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x) (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 1.7

;;; failure for large number

(sqrt 1e7)

;;; failure for small number

(sqrt 1e-7)

;;; new good-enough?

(define (rel-good-enough? prev-guess guess)
  (< (abs (- guess prev-guess))
     (* guess 0.0001)))			; relative to total size

(define (rel-sqrt-iter prev-guess guess x)
  (if (rel-good-enough? prev-guess guess)
      guess
      (rel-sqrt-iter guess
		     (improve guess x)
		     x)))

(define (sqrt x)
  (rel-sqrt-iter 0 1.0 x))

(sqrt 1000000)
(sqrt 1e-7)
(sqrt 9)

;;; fairly average results at best

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 1.8

(define (cube-root x)
  (define (cube-root-iter guess)
    (if (cube-good-enough? guess x)
	guess
	(cube-root-iter (n-improve guess))))
  (define (cube-good-enough? guess x)
    (< (abs (- (cube guess) x)) 0.001))
  (define (cube x)
    (* x x x))
  (define (n-improve y)
    (/ (+ (/ x (square y)) (* 2 y)) 3))
  (cube-root-iter 1.0))

(cube-root 1000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 1.9

;;; first is recursive
;;; second is iterative

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 1.10

;; https://en.wikipedia.org/wiki/Ackermann_function

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 1.11
;;; recursive
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
	 (* 2 (f (- n 2)))
	 (* 3 (f (- n 3))))))

(f 1)
(f 2)
(f 3)
(f 10)
(f 20)
;; (f 40) <- blows up!

;;; iterative

(define (f2 n)
  (define (f-iter a_n-1 a_n-2 a_n-3 n)
  (if (= 0 n)
      a_n-1
      (f-iter (+ a_n-1 (* 2 a_n-2) (* 3 a_n-3))
	      a_n-1
	      a_n-2
	      (- n 1))))
  (if (< n 3)
      n
      (f-iter 2 1 0 (- n 2))))		;starting values from <3

(f2 1)
(f2 2)
(f2 3)
(f2 10)
(f2 20)
(f2 40)					;no problem

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1.12

(define (pascal row elem)
  (if (or (= row 1) (= elem 1) (= elem row))
      1
      (+ (pascal (- row 1) (- elem 1))
	 (pascal (- row 1) elem))))

(pascal 1 1)
(pascal 4 3)
(pascal 5 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1.13,14,15

;;; do when I have some paper on me...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1.16
;;; expt from book:

(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

(fast-expt 3 9)

;;; iterative
(define (efficient-expt b n)
  (define (iter-expt a b n)
    (cond ((= n 0)  a)
	  ((even? n) (iter-expt a (square b) (/ n 2)))
	  (else (iter-expt (* a b) b (- n 1)))))
  (iter-expt 1 b n))

(efficient-expt 2 2)
(efficient-expt 3 9)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1.17
(define (double x)
  (* x 2))

(define (halve x)
  (/ x 2))

;; ;;; first attempt
;; (define (odd? x)
;;   (= (remainder x 2) 1))

;; (define (mult a b)
;;   (cond ((= a 1) b)
;; 	((odd? a) (+ b (mult (- a 1) b)))
;; 	(else (mult (halve a) (double b)))))

(define (mult a b)
  (cond ((= b 1) a)
	((even? b) (mult (double a) (halve b)))
	(else (+ a (mult a (- b 1))))))

(mult 3 4)
(mult 100 60)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1.18

;;; above?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1.29

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (simpsons-rule f a b n)
  (define h (/ (- b a) n))
  (define (incr x)
    (+ x 1))
  (define (y k)
    (cond ((= 0 k) (f a))
	  ((= n k) (f (+ a (* h n))))
	  ((even? k) (* 2 (f (+ a (* h k)))))
	  (else (* 4 (f (+ a (* h k)))))))
  (* (/ h 3)
     (sum y 0 incr n)))

(simpsons-rule cube 0 1 100)
(simpsons-rule cube 0 1 1000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1.30

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ (term a) result))))
  (iter a 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1.31

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorial n)
  (define (identity x) x)
  (define (incr x)
    (+ x 1))
  (product identity 1 incr n))

(factorial 5)
(* 5 4 3 2 1)

(define (pi-approx n)
  (define (frac-k k)
    (if (even? k)
	(/ (+ k 2) (+ k 1))
	(/ (+ k 3) (+ k 2))))
    (define (++ x)
    (+ x 1))
    (* (/ 2 (+ n 1))
       (product frac-k 1.0 ++ n)))

(* 4 (pi-approx 1e6))			;nice!!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1.32

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (sum term a next b)
  (accumulate + 0 term a next b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1.33

(define (filtered-accumulate filter combiner null-value term a next b)
  (define (filtered-combine x y)
    (if (filter x)
	(combiner x y)
	y))
  (accumulate filtered-combine null-value term a next b))

(define (++ x)
  (+ x 1))

(define (id x) x)

;;; sum of even numbers (for testing)
(filtered-accumulate even? + 0 id 1 ++ 10)
(+ 2 4 6 8 10)

;;; a

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))
(define (prime? n)
  (fast-prime? n 20))
(prime? 8)

(define (ss-primes a b)
  (filtered-accumulate prime? (lambda (x y) (+ (square x) y)) 0 id a ++ b))

;;; The trick is to not have the term parameter be square! Put
;;; squaring in the combiner instead. Then the trick is to combine in
;;; the right order!

(ss-primes 2 10)
(+ (square 2) (square 3) (square 5) (square 7))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1.35

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1.36

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display guess)
      (newline)
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)

(expt 4.555532270803653 4.555532270803653)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1.37
;;; a. recursive - increasing i
(define (cont-frac n d k)
  (define (recurse i)
    (if (= i k)
	(/ (n k) (d k))
	(/ (n i) (+ (d i) (recurse (+ i 1))))))
  (recurse 1))

(/ 1 
   (cont-frac (lambda (i) 1.0)
	      (lambda (i) 1.0)
	      1000))

;;; b. iterative - decreasing i
(define (cont-frac n d k)
  (define (iter total i)
    (if (= i 0)
	total
	(iter (/ (n i) (+ (d i) total)) (- i 1))))
  (iter 0 k))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1.38

(+ 2 (cont-frac (lambda (i) 1.0)
		(lambda (i)
		  (if (= 2 (remainder i 3))
		      (* 2 (/ (+ i 1) 3))
		      1))
		1000))

(define (seq start finish f)
  (define (iter i)
    (display (f i))
    (display " ")
    (if (= i finish)
	()
	(iter (++ i))))
  (iter start))


(seq 1 10 (lambda (i) i))

(seq 1 21 (lambda (i)
		  (if (= 2 (remainder i 3))
		      (* 2 (/ (+ i 1) 3))
		      1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1.40

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

((deriv cube) 5)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
		  1.0))

(sqrt 9)

(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

(define (root-cubic a b c)
  (newtons-method (cubic a b c) 1))

(root-cubic 1 2 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1.41

(define (double f)
  (lambda (x)
    (f (f x))))

((double ++) 4)

(((double (double double)) ++) 5)	;apply inc (++) (2^2)^2 to 5

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1.42

(define (compose f g)
  (lambda (x)
    (f (g x))))

((compose square ++) 6)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1.43

(define (repeated f n)
  (if (= n 1)
      f
      (compose (repeated f (- n 1)) f)))

((repeated square 2) 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1.44

(avg-3 1 2 3)

(define (avg-3 x y z)
  (/ (+ x y z) 3))

(average (average 1 2) 3)

(define (smooth f dx)
  (lambda (x)
    (average (- (f x) dx)
	     (f x)
	     (+ (f x) dx))))

(define (n-smooth f dx n)
  (repeated (smooth f dx) n))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1.46

(define (iterative-improve test-good-enough improve-guess)
  (lambda (guess x)
    (if (test-good-enough guess x)
	guess
	((iterative-improve test-good-enough improve-guess)
	 (improve-guess guess x) x))))

(define (general-sqrt guess x)
  ((iterative-improve good-enough? improve) guess x))

(define (sqrt x)
  (general-sqrt (/ x 2) x))

(sqrt 9.0)

;;;;;;      ;;;;;;      ;;;;;;      ;;;;;;      ;;;;;;
      ;;;;;;      ;;;;;;      ;;;;;;      ;;;;;;
;;;;;;      ;;;;;;      *DONE*      ;;;;;;      ;;;;;;
      ;;;;;;      ;;;;;;      ;;;;;;      ;;;;;;
;;;;;;      ;;;;;;      ;;;;;;      ;;;;;;      ;;;;;;
