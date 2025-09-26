(module pythagorean racket/base
  (require racket/contract (rename-in Scheme+ (-> =->)))
  (require (only-in racket/list first))
  (require (only-in math divisors))
  (require (only-in racket range))
  (provide triplets-with-sum)
  (define (get-triplets-with-hypotenuse n)
    (for/list
     ((a (in-range 1 (/ n (√ 2))))
      #:do
      ((<- b (√ (- (* n n) (* a a)))))
      #:when
      (integer? b))
     (list a b)))
  (define (positive-integer? n) (and (positive? n) (integer? n)))
  (define (pythagorean-triplet? a b c)
    (and (< a b c) (equal? (+ (* a a) (* b b)) (* c c))))
  (define (positive-integer-triplet? t)
    (and (list? t)
         (equal? (length t) 3)
         (positive-integer? (first t))
         (apply pythagorean-triplet? t)))
  (define/contract
   (triplets-with-sum-brute-force n)
   (-> positive-integer? (listof positive-integer-triplet?))
   (if (odd? n)
     '()
     (apply
      append
      (for/list
       ((a (in-range 1 (/ n 3))))
       (for/list
        ((b (in-range (add1 a) (/ (* n 2) 3)))
         #:do
         ((<- c (- (- n a) b)))
         #:when
         (pythagorean-triplet? a b c))
        (list a b c))))))
  (define (coprime? . args) (equal? (apply gcd args) 1))
  (define (m-and-n-valid? m n) (and (odd? (+ m n)) (coprime? m n)))
  (define (primitive-triples-with-perimeter p)
    (if (even? p)
      then
      (:= s (/ p 2))
      (for/list
       ((m (divisors s))
        #:when
        (< (√ (/ s 2)) m (√ s))
        #:do
        ((def n s / m - m))
        #:when
        (m-and-n-valid? m n)
        #:do
        ((def a m * m - n * n) (def b 2 * m * n) (def c p - (a + b))))
       (list (min a b) (max a b) c))
      else
      '()))
  (define has-triplets
    (list
     12
     24
     30
     36
     40
     48
     56
     60
     70
     72
     80
     84
     90
     96
     108
     112
     120
     126
     132
     140
     144
     150
     154
     156
     160
     176
     180
     182
     192
     198
     200
     204
     208
     210
     216
     220
     224
     228
     234
     240
     252
     260
     264
     270
     276
     280
     286
     288
     300
     306
     308
     312
     320
     324
     330
     336
     340
     348
     350
     352
     360
     364
     372
     374
     378
     380
     384
     390
     392
     396
     400
     408
     416
     418
     420
     432
     440
     442
     444
     448
     450
     456
     462
     468
     476
     480
     490
     492
     494
     504
     510
     516
     520
     528
     532
     540
     544
     546
     552
     560
     564
     570
     572
     576
     588
     594
     598
     600
     608
     612
     616
     624
     630
     636
     640
     644
     646
     648
     650
     660
     672
     680
     684
     690
     696
     700
     702
     704
     708
     714
     720
     728
     732
     736
     744
     748
     750
     756
     760
     768
     770
     780
     782
     784
     792
     798
     800
     804
     810
     816
     828
     832
     836
     840
     850
     852
     858
     864
     870
     874
     876
     880
     882
     884
     888
     896
     900
     910
     912
     918
     920
     924
     928
     930
     936
     948
     950
     952
     960
     966
     972
     980
     984
     986
     988
     990
     992
     996
     1000
     1008
     1012
     1020
     1026
     1032
     1040
     1044
     1050
     1054
     1056
     1064
     1068))
  (define/contract
   (triplets-with-sum p)
   (-> positive-integer? (listof positive-integer-triplet?))
   (if (odd? p)
     '()
     else
     (:=
      triples
      (for/fold
       ((triples '()))
       ((d (divisors p))
        #:do
        ((define primitive-triples (primitive-triples-with-perimeter d))
         (def scale p / d)))
       (append
        (for/list
         ((triple primitive-triples))
         (for/list ((leg triple)) (* scale leg)))
        triples)))
     (sort triples #:key first <)))
  (module+ test (require rackunit))
  (module+
   test
   (test-equal? "provided example" (triplets-with-sum 12) '((3 4 5)))
   (test-equal? "empty list" (triplets-with-sum 13) '())
   (test-equal?
    "first perimeter with three triplets"
    (triplets-with-sum 120)
    '((20 48 52) (24 45 51) (30 40 50)))
   (test-equal?
    "given tests include 30000, let's be even more ambitious"
    (triplets-with-sum 40000)
    '((8000 15000 17000) (8750 14400 16850)))
   (check-exn exn:fail? (λ () (triplets-with-sum 0)) "zero -> BOOM")
   (check-exn
    exn:fail?
    (λ () (triplets-with-sum 1.23))
    "not an integer -> BOOM")))
