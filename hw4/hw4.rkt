
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
(define (sequence low high stride)
    (if (> low high)
        null
        (cons low (sequence (+ low stride) high stride))))


(define (string-append-map xs suffix)
    (map (lambda (x) (string-append x suffix)) xs))


(define (list-nth-mod xs n)
    (cond [(< n 0) (error "list-nth-mod: negative number")]
          [(null? xs) (error "list-nth-mod: empty list")]
          [#t (car (list-tail xs (remainder n (length xs))))]))


(define (stream-for-n-steps s n)
    (if (= n 0)
        null
        (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))


(define funny-number-stream
    (letrec ([f (lambda (x) (cons (if (= (remainder x 5) 0) (- x) x) 
                                  (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))


(define dan-then-dog
    (letrec ([dan (lambda () (cons "dan.jpg" dog))]
             [dog (lambda () (cons "dog.jpg" dan))])
    (lambda () (dan))))


(define (stream-add-zero s)
    (lambda () (cons (cons 0 (car (s))) (stream-add-zero (cdr (s))))))


(define (cycle-lists xs ys)
    (letrec ([f (lambda (n) (cons (cons (list-nth-mod xs n)
                                        (list-nth-mod ys n)) 
                                  (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))


(define (vector-assoc v vec)
    (letrec ([f (lambda(i) (cond [(= i (vector-length vec)) #f]
                                  [(and (pair? (vector-ref vec i)) (equal? v (car (vector-ref vec i)))) (vector-ref vec i)]
                                  [#t (f (+ i 1))]))])
    (f 0)))


(define (cached-assoc xs n)
    (letrec ([memo (make-vector n #f)]
            [i 0])
    (lambda (x)
        (or (vector-assoc x memo)
            (let ([res (assoc x xs)])
                (and res (begin
                         (vector-set! memo i res)
                         (set! i (remainder (+ i 1) n))
                          res)))))))