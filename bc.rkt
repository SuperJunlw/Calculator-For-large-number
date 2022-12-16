#lang racket

(require "big-num.rkt")

;; Crude workaround to split math expressions easily
(define (bc-split exp)
  (string-split
   (string-replace
    (string-replace
     (string-replace
      (string-replace exp "+" " + ")
      "-" " - ")
     "*" " * ")
    "^" " ^ ")))

(define (perform-op lst)
  (let ([arg1 (string->bignum (car lst))]
        [op (cadr lst)]
        [arg2 (string->bignum (caddr lst))])
    (display (cond
               [(string=? op "+") (pretty-print (big-add arg1 arg2))]
               [(string=? op "-") (pretty-print (big-subtract arg1 arg2))]
               [(string=? op "*") (pretty-print (big-multiply arg1 arg2))]
               [(string=? op "^") (pretty-print (big-power-of arg1 arg2))]
               [else "Not supported"])))
  (newline))
  
;; Very simple parser
(for ([l (in-lines)])
  (let ([words (bc-split l)])
    (cond [(= 0 (length words)) (void)] ;; Ignore empty lines
          [(= 3 (length words)) (perform-op words)]
          [else (display "Complex expressions not supported")])))
