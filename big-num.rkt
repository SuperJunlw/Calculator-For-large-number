#lang racket

;;Jun Wu - CS152 - HW1
;;Sep 17, 2022

;; The big-num data structure is essentially a list of 3 digit numbers.

;; Exporting methods
(provide big-add big-subtract big-multiply big-power-of big-eq pretty-print
         number->bignum string->bignum bignum? zero-or-one? one-block?)

(define MAX_BLOCK 1000)

;; Contract verifying the datatype of a bignum
(define (bignum? n)
  (cond [(not (list? n)) #f]
        [(not (list-of-ints? n)) #f]
        [else #t]))

;; Helper contract
(define (list-of-ints? lst)
  (cond [(empty? lst) #t]
        [(integer? (car lst)) (list-of-ints? (cdr lst))]
        [else #f]))

;; Contract to ensure a number is 0 or 1.
(define (zero-or-one? n)
  (match n
    [0 #t]
    [1 #t]
    [_ #f]))

;; Contract to insure a number is an integer in the range of 0-999.
(define (one-block? n)
  (and (integer? n)
       (>= n 0)
       (< n 1000)))

;; Addition of two big-nums
(define/contract (big-add x y)
  (-> bignum? bignum? bignum?)
  (big-add1 x y 0)
  )

(define/contract (big-add1 x y co)
  (-> bignum? bignum? zero-or-one? bignum?)
  (cond
    ;; If both lists are empty, the return value is either 0 or the caryover value.
    [(and (= 0 (length x)) (= 0 (length y)))
      (if (= co 0) '() (list co))]
    [(= 0 (length x))  (big-add1 (list co) y 0)]
    [(= 0 (length y))  (big-add1 x (list co) 0)]
    [else
       ;;
       ;; --- YOUR CODE HERE ---
       ;;
     (let* ([res (+ (car x) (car y) co)]
       [digit (remainder res 1000)]
       [c (quotient res 1000)])
       (cons digit (big-add1 (cdr x) (cdr y) c))
       )]))
    

;; Subtraction of two big-nums
(define/contract (big-subtract x y)
  (-> bignum? bignum? bignum?)
  (let ([lst (big-subtract1 x y 0)])
    (reverse (strip-leading-zeroes (reverse lst)))
  ))

(define/contract (strip-leading-zeroes x)
  (-> bignum? bignum?)
  (cond
    [(= 0 (length x)) '(0)]
    [(= 0 (car x)) (strip-leading-zeroes (cdr x))]
    [else x]
    ))

;; NOTE: there are no negative numbers with this implementation,
;; so 3 - 4 should throw an error.
(define/contract (big-subtract1 x y borrow)
  (-> bignum? bignum? zero-or-one? bignum?)
  ;;
  ;; --- YOUR CODE HERE ---
  ;;
  (cond
     [(and (= 0 (length x)) (= 0 (length y)))
      (if (= borrow 0) '() (error "Result is negative"))]
    [(= 0 (length x)) (error "Result is negative")]
    [(= 0 (length y))  (big-subtract1 x (list 0) borrow)]
    [else
      (let* ([res (- (car x) (car y) borrow)]
       [digit (if (< res 0) (+ res 1000) res)]
       [b (if(< res 0) 1 0)])
       (cons digit (big-subtract1 (cdr x) (cdr y) b))
       )]))
 

;; Returns true if two big-nums are equal
(define/contract (big-eq x y)
  (-> bignum? bignum? boolean?)
  ;;
  ;; --- YOUR CODE HERE ---
  ;;
  (cond
     [(and (= 0 (length x)) (= 0 (length y))) true]
    [(= 0 (length x)) false]
    [(= 0 (length y)) false]
    [(= (car x) (car y)) (big-eq (cdr x) (cdr y))]
    [else false])
  )

;; Decrements a bignum
(define/contract (big-dec x)
  (-> bignum? bignum?)
  (big-subtract x '(1))
  )

;; Multiplies two big-nums
(define/contract (big-multiply x y)
  (-> bignum? bignum? bignum?)
  ;;
  ;; --- YOUR CODE HERE ---
  ;;
  
  ;; Follow the same approach that you learned in
  ;; grade school for multiplying numbers, except
  ;; that a "block" is 0-999, instead of 0-9.
  ;; Consider creating a helper function that multiplies
  ;; a big-number with a integer in the range of 0-999.
  ;; Once you have that working, you can use it in your
  ;; solution here.
  (if (or (and (null? (cdr x)) (= (car x) 0)) (and (null? (cdr y)) (= (car y) 0))) ;;check if x or y is zero
      (list 0) 
      (big-multiply-helper x y (length y))           
  ))

;;helper of big multiply
(define (big-multiply-helper x y yLength)
  (cond
    [(= 0 (length y)) (list 0)]
    [else
     (let* ([res (one-multiply x (car y) 0)]  ;;one multiplication
            [resAddZero (add-zeros res (- yLength (length y)))]) ;;add zeros for the addition
            (big-add resAddZero (big-multiply-helper x (cdr y) yLength)))]))

            
;;helper, multiply list of numbers to a number
(define (one-multiply x y co)
  (cond
    [ (= 0 (length x)) (if (= co 0) '() (list co))]
    [else (let* ([res (+ (* (car x) y) co)]
                 [digit (remainder res 1000)]
                 [c (quotient res 1000)])
            (cons digit (one-multiply (cdr x) y c)))]))

;;helper, add zeros to front of the list
(define (add-zeros res num-zeros)
  (if (= num-zeros 0)
      res
      (cons 0 (add-zeros res (- num-zeros 1)))))

  
;; Raise x to the power of y
(define/contract (big-power-of x y)
  (-> bignum? bignum? bignum?)
  ;;
  ;; --- YOUR CODE HERE ---
  ;;
  
  ;; Solve this function in terms of big-multiply.
  (cond
    [(= (car y) 0) (list 1)]
    [else (big-multiply x (big-power-of x (list (- (car y) 1))))]
  ))

;; Dispaly a big-num in an easy to read format
(define (pretty-print x)
  (let ([lst (reverse x)])
    (string-append
     (number->string (car lst))
     (pretty-print1 (cdr lst))
     )))

(define (pretty-print1 x)
  (cond
    [(= 0 (length x))  ""]
    [else
     (string-append (pretty-print-block (car x)) (pretty-print1 (cdr x)))]
    ))

(define (pretty-print-block x)
  (string-append
   ","
   (cond
     [(< x 10) "00"]
     [(< x 100) "0"]
     [else ""])
   (number->string x)))

;; Convert a number to a bignum
(define/contract (number->bignum n)
  (-> number? bignum?)
  (cond
    [(< n MAX_BLOCK) (list n)]
    [else
     (let ([block (modulo n MAX_BLOCK)]
           [rest (floor (/ n MAX_BLOCK))])
       (cons block (number->bignum rest)))]))

;; Convert a string to a bignum
(define/contract (string->bignum s)
  (-> string? bignum?)
  (let ([n (string->number s)])
    (number->bignum n)))