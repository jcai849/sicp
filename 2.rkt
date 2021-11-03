#lang sicp

; 2.1
(define (gcd a b)
 (if (= b 0)
      a
    (gcd b (remainder a b))))

(define (print-rat x)
    (newline)
    (display (numer x))
    (display "/")
    (display (denom x)))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (make-rat n d)
    (let ((np (if (< (* n d) 0) (- (abs n)) (abs n)))
          (dp (abs d)))
        (let ((g (abs (gcd np dp))))
            (cons (/ np g) (/ dp g)))))

; 2.2

(define (make-segment start end)
    (cons start end))
(define (start-segment segment)
    (car segment))
(define (end-segment segment)
    (cdr segment))

(define (make-point x y)
    (cons x y))
(define (x-point point)
    (car point))
(define (y-point point)
    (cdr point))

(define (midpoint-segment segment)
    (make-point (/ (+ (x-point (start-segment segment))
                      (x-point (end-segment segment)))
                    2)
                (/ (+ (y-point (start-segment segment))
                      (y-point (end-segment segment)))
                    2)))

(define (print-point p)
    (newline)
    (display "(")
    (display (x-point p))
    (display ",")
    (display (y-point p))
    (display ")"))

; 2.3

(define (make-rectangle bottom-left top-right)
    (cons bottom-left top-right))
(define (bottom-left rectangle)
    (car rectangle))
(define (top-right rectangle)
    (cdr rectangle))

(define (measure dimension)
    (lambda (rectangle)
        (abs (- (dimension (top-right rectangle))
                (dimension (bottom-left rectangle))))))
;(define height (measure y-point))
;(define width (measure x-point))

(define (perimeter rectangle)
    (+ (* 2 (height rectangle))
       (* 2 (width rectangle))))
(define (area rectangle)
    (* (height rectangle)
       (width rectangle)))

; ii

(define (make-alt-rectangle x-segment y-segment)
    (cons x-segment y-segment))
(define (x-segment alt-rectangle)
    (car alt-rectangle))
(define (y-segment alt-rectangle)
    (cdr alt-rectangle))
(define (alt-measure point-dimension segment-dimension)
    (lambda (alt-rectangle)
        (abs (- (point-dimension (start-segment (segment-dimension alt-rectangle)))
                (point-dimension (end-segment (segment-dimension alt-rectangle)))))))
(define height (alt-measure y-point y-segment))
(define width (alt-measure x-point x-segment))

; 2.4

(define (proc-cons x y)
    (lambda (m) (m x y)))
(define (proc-car z)
    (z (lambda (p q) p)))

(define (proc-cdr z)
    (z (lambda (p q) q)))

(let ((x 1)
      (y 2))
    (display (proc-car (proc-cons x y)))
    (newline)
    (display (proc-car (lambda (m) (m x y))))
    (newline)
    (display ((lambda (m) (m x y)) (lambda (p q) p)))
    (newline)
    (display ((lambda (p q) p) x y))
    (newline)
    (display x)
    (newline)
    (display (proc-cdr (proc-cons x y))))

; 2.5

(define (expt b n)
    (expt-iter b n 1))
(define (expt-iter b counter product)
    (if (= counter 0)
        product
        (expt-iter b
                  (- counter 1)
                  (* b product))))

(define (parity? rem)
    (lambda (n)
        (= (remainder n 2) rem)))
(define even? (parity? 0))
(define odd? (parity? 1))
(define (num-cons a b)
    (* (expt 2 a)
       (expt 3 b)))
(define (mod-log divisor parity?)
    (define (accumulator n count)
                (if (and (parity? n) (> n 1))
                    (accumulator (/ n divisor) (+ count 1))
                    count))
    accumulator)
(define (num-car x)
    (let ((mod-log2 (mod-log 2 even?)))
         (mod-log2 x 0)))
(define (num-cdr x)
    (let ((mod-log3 (mod-log 3 odd?)))
        (if (even? x)
            (num-cdr (/ x 2))
            (mod-log3 x 0))))
