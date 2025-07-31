#lang racket/base


;; This is lovely but Exercism doesn't let you (require math)!
;;
;; Posted this: https://gist.github.com/dandrake/d9a3e20021a68ed16bce6f5dc79e62eb



(require racket/contract)

;; I'm trying to stick to racket/base as much as possible. I know
;; there's car; I just like "first".
(require (only-in racket/list first))

(require (only-in math divisors))

;; Use this to run 'time'; you want the for-each but that needs range,
;; not in-range.
(require (only-in racket range))

(provide triplets-with-sum)

;; I wrote this first before reading the problem carefully. But it works
;; and I don't want to delete it. 🙂
(define (get-triplets-with-hypotenuse n)
  (for/list ([a (in-range 1 (/ n (sqrt 2)))]
             #:do [(define b (sqrt (- (* n n) (* a a))) )]
             #:when (integer? b))
    (list a b)))


;; ====================================
;;
;; Brute force search
;;
;; Very simple, but slower.
;;
;; ====================================


;; It would be simpler and easier to just use the built-in
;; nonnegative-integer? but being strict about the problem, which says
;; positive integer, so we have to make a predicate for our contract.
(define (positive-integer? n) (and (positive? n) (integer? n)))

;; And while we're at it, just to be ridiculously pedantic, let's be
;; very precise about what we promise to return:
;;
;; - a list of...
;; - lists of length 3...
;; - of positive integers...
;; - in increasing order...
;; - that are Pythagorean triples.


;; the list we promise to return: not just a list of
;; triples of positive integers that are Pythagorean triples, listed in
;; increasing order.

(define (pythagorean-triplet? a b c)
  (and (< a b c)
       (equal? (+ (* a a) (* b b)) (* c c))))

(define (positive-integer-triplet? t)
    (and (list? t)
         (equal? (length t) 3)
         (positive-integer? (first t))
         (apply pythagorean-triplet? t)))

(define/contract (triplets-with-sum-brute-force n)
  (positive-integer? . -> . (listof positive-integer-triplet?))
  (if (odd? n)
      ;; odd perimeter never has any triplets
      '()
      (apply append (for/list ([a (in-range 1 (/ n 3))])
                      (for/list ([b (in-range (add1 a) (/ (* n 2) 3))]
                                 #:do [(define c (- n a b))]
                                 #:when (pythagorean-triplet? a b c))
                        (list a b c))))))

#|

Or, use number theory to optimize the search.

We use Euclid's formula to get primitive Pythagorean triplets: for all
m and n:

a = m^2 - n^2, b = 2mn , c = m^2 + n^2

is a triplet. Adding those you get 2m^2 + 2mn = 2m(m + n) for the
perimeter. So, half the perimeter must factor into m(m + n). In
particular, m is a divisor of p/2.

So we just find divisors of p/2, and for each of those, find n: it's
(p/2)/m - m. If that's an integer, m and n give you the triplet.

For the basic 3,4,5: you find divisors of 6. The "m, n" pairs are:

'((1 . 5) (2 . 1) (3 . -1) (6 . -5))

filtering out ones where n is negative or bigger than m gives you the
triplets you want; to get a positive n, you need m < sqrt(p/2); and
for a positive n, you need m > sqrt((p/2)/2). Together, you have

sqrt(p/4) < m < sqrt(p/2). That's the #:when below.

(Aside: I usually am not a big fan of prefix notation for
inequalities; my little pea brain thinks visually and wants the
pointy end pointing to the smaller number -- or, equivalently, the
alligator eating the larger number. But prefix notation makes it very
slick to check for a monotonic sequence with just a single < or >.)

But! Two problems:

First, that gives you some non-primitive triplets, in which a, b, and c
aren't relatively prime. Second, it doesn't give you all triplets!

So we sharpen the above to output only primitive triplets: see
valid-m-and-n? below.

Then, given a perimeter: every triplet with that perimeter can be
obtained by scaling up the primitive triplets for some divisor of p.

|#

(define (coprime? . args) (equal? (apply gcd args) 1))
;; valid m and n: must be relatively prime (otherwise, obviously, the
;; resulting a,b,c will be divisible by the same factor); also, you need
;; exactly one of m and n to be odd. If both even, well, they're not
;; coprime; if both odd, you get a,b,c all even.
(define (m-and-n-valid? m n)
  (and (odd? (+ m n))
       (coprime? m n)))

;; I don't have a proof, but for a given perimeter, there never seems to
;; be multiple primitive triplets. Tested up to 10^7.
(define (primitive-triples-with-perimeter p)
  (if (even? p)
      (let ([s (/ p 2)])
        (for/list ([m (divisors s)]
                   #:when (< (sqrt (/ s 2))
                             m
                             (sqrt s))
                   #:do [(define n (- (/ s m) m))]
                   #:when (m-and-n-valid? m n)
                   #:do [(define a (- (* m m) (* n n)))
                         (define b (* 2 m n))
                         (define c (- p (+ a b)))])
          (list (min a b) (max a b) c)))
      '()))

;; For reference/testing: all the perimeters up to 1000ish that have
;; triplets. 120 is the first with 3; 240 is the first with 4; 420 is
;; the first with 5; 1440 is the first with 6.
(define has-triplets (list 12 24 30 36 40 48 56 60 70 72 80 84 90 96 108
                           112 120 126 132 140 144 150 154 156 160 176 180
                           182 192 198 200 204 208 210 216 220 224 228 234
                           240 252 260 264 270 276 280 286 288 300 306
                           308 312 320 324 330 336 340 348 350 352 360
                           364 372 374 378 380 384 390 392 396 400 408
                           416 418 420 432 440 442 444 448 450 456 462
                           468 476 480 490 492 494 504 510 516 520 528
                           532 540 544 546 552 560 564 570 572 576 588
                           594 598 600 608 612 616 624 630 636 640 644
                           646 648 650 660 672 680 684 690 696 700 702
                           704 708 714 720 728 732 736 744 748 750 756
                           760 768 770 780 782 784 792 798 800 804 810
                           816 828 832 836 840 850 852 858 864 870 874
                           876 880 882 884 888 896 900 910 912 918 920
                           924 928 930 936 948 950 952 960 966 972 980
                           984 986 988 990 992 996 1000 1008 1012 1020
                           1026 1032 1040 1044 1050 1054 1056 1064 1068))

#|

Use something like this to compare times:

: (time (for-each (lambda (_) (triplets-with-sum-brute-force p)) (range (expt 10 3))))

Some results from doing

: (time (for-each (lambda (_) (triplets-with-sum p)) (range (expt 10 4))))

for various values of p. Milliseconds of CPU time on my machine:

|    p | brute force | number theoretic | speedup factor |
|------+-------------+------------------+----------------|
|  120 |         881 |              327 |  2.69418960245 |
|  240 |        3370 |              445 |  7.57303370787 |
|  420 |       10218 |              498 |  20.5180722892 |
|  480 |       13441 |              683 |  19.6793557833 |
| 1000 |       74196 |              357 |  207.831932773 |

For 1000, there's only one triplet; perhaps unsurprisingly, it seems the
number-theoretic algorithm is good at eliminating unnecessary
computation early.

|#

(define/contract (triplets-with-sum p)
  (positive-integer? . -> . (listof positive-integer-triplet?))
  (if (odd? p)
      ;; odd perimeter never has any triplets
      '()
      (let ([triples (for/fold ([triples '()])
                               ([d (divisors p)]
                                #:do [(define primitive-triples (primitive-triples-with-perimeter d))
                                      (define scale (/ p d))])
                       (append (for/list ([triple primitive-triples])
                                 (for/list ([leg triple]) (* scale leg)))
                               triples))])
        (sort triples #:key first <))))

;; (We have to sort because the provided tests assume we return a sorted
;; list.)


;; I don't have a proof, but it seems like you never get multiple
;; primitive triplets for a given perimeter; I've tested up to 10^7.
#;(define (foo3 [start 1000] [stop 2000])
  (for/fold ([result '("start")])
            ([s (foo start stop)]) ;; foo is something that counts return values from primitive-triplets-with-perimeter
    (unless (equal? (string-length s) 0)
      (cons s result))))

(module+ test
  (require rackunit))

(module+ test
  (test-equal? "provided example"
               (triplets-with-sum 12)
               '((3 4 5)))
  (test-equal? "empty list"
               (triplets-with-sum 13)
               '())
  (test-equal? "first perimeter with three triplets"
               (triplets-with-sum 120)
               '((20 48 52) (24 45 51) (30 40 50)))
  (test-equal? "given tests include 30000, let's be even more ambitious"
               (triplets-with-sum 40000)
               '((8000 15000 17000) (8750 14400 16850)))
  (check-exn exn:fail?
             (λ () (triplets-with-sum 0))
             "zero -> BOOM")
  (check-exn exn:fail?
             (λ () (triplets-with-sum 1.23))
             "not an integer -> BOOM"))
