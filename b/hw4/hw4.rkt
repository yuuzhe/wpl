
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; Q1
(define (sequence low high stride)
  (if (>= low (+ high 1))
    null
    (cons low (sequence (+ low stride) high stride))))

;; Q2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;; Q3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

;; Q4
;; Care about the stream side affect!!!
;; Should let it evaluate and save it in a local variable.
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))
;; Q5
(define (funny-number-stream)
  (define (f x) (cons (if (= (remainder x 5) 0) (* x -1) x)
                      (lambda () (f (+ x 1)))))
  (f 1))

;; Q6
(define (dan-then-dog)
  (define (f is-dan) (cons (if is-dan "dog.jpg" "dan.jpg")
                           (lambda () (f (not is-dan)))))
  (f #f))

;; Q6 mutual recursive version
(define (dan-then-dog2)
  (letrec ([dan (lambda () (cons "dan.jpg" (lambda () (dog))))]
           [dog (lambda () (cons "dog.jpg" (lambda () (dan))))])
    (dan)))

;; Q7
;; Care about the stream side affect!!!
;; Should let it evaluate and save it in a local variable.
(define (stream-add-zero s)
  (lambda ()
    (cons (cons 0 (car (s)))
          (lambda () ((stream-add-zero (cdr (s))))))))

;; Q8
(define (cycle-lists xs ys)
  (define (f n)
    (cons (cons (list-ref xs (remainder n (length xs)))
                (list-ref ys (remainder n (length ys))))
          (lambda () (f (+ n 1)))))
  (lambda () (f 0)))

;; Q9
(define (vector-assoc v vec)
  (define (f index)
    (if (= index (vector-length vec))
        #f
        (let ([pair (vector-ref vec index)])
          (if (and (pair? pair) (equal? (car pair) v))
              pair
              (f (+ index 1))))))
  (f 0))

;; Q10
(define (cached-assoc xs n)
  (define cache (make-vector n #f))
  (define pos 0)
  (define (f v)
    (let ([result (vector-assoc v cache)])
      (if result
          result
          (let ([new-result (assoc v xs)])
            (if new-result
                (begin (vector-set! cache pos new-result)
                       (set! pos (remainder (+ pos 1) n))
                       new-result)
                #f)))))
  f)
