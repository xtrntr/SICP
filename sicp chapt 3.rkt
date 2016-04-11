#lang racket

(require rackunit)

;ex 3.1
(define (make-accumulator init)
  (let ([store init])
    (lambda (to-add)
      (set! store (+ store to-add))
      store)))
(define A (make-accumulator 5))
(check-equal? (A 10) 15)
(check-equal? (A 10) 25)

;ex 3.2
(define (make-monitored fn)
  (let ([calls 0])
    (lambda (arg)
      (cond [(eq? arg 'how-many-calls?) (set! calls (add1 calls)) calls]
            [else (fn arg)]))))
       
(define s (make-monitored sqrt))
(check-equal? (s 100) 10)
(check-equal? (s 'how-many-calls?) 1)

;ex 3.3
(define (make-account init-val pass)
  (let ([balance init-val])
    (lambda (inp-pass command)
      (if (eq? inp-pass pass)
          (cond ([eq? command 'withdraw] (lambda (to-withdraw) (set! balance (- balance to-withdraw)) balance))
                ([eq? command 'deposit] (lambda (to-deposit) (set! balance (+ balance to-deposit)) balance)))
          (lambda (x) "Incorrect password")))))
(define acc (make-account 100 'secret-password))
(check-equal? ((acc 'secret-password 'withdraw) 40) 60)
(check-equal? ((acc 'some-other-password 'deposit) 50) "Incorrect password")

;ex 3.4
(define (make-account2 init-val pass)
  (let ([balance init-val]
        [incorrect-attempts 0])
    (lambda (inp-pass command)
      (if (eq? inp-pass pass)
          (begin
            (cond ([eq? command 'withdraw] (lambda (to-withdraw) (set! balance (- balance to-withdraw)) balance))
                  ([eq? command 'deposit] (lambda (to-deposit) (set! balance (+ balance to-deposit)) balance)))
            (set! incorrect-attempts 0))
          (lambda (x) (if (> incorrect-attempts 7)
                          "Callin' the cops"
                          (begin (set! incorrect-attempts (add1 incorrect-attempts))
                                 "Incorrect password")))))))
(define acc2 (make-account2 100 'secret-password))
(for ([i (range 8)])
     (check-equal? ((acc2 'some-other-password 'deposit) 50) "Incorrect password"))
(check-equal? ((acc2 'some-other-password 'deposit) 50) "Callin' the cops")

;ex 3.5
(define (monte-carlo trials experiment)
  (let loop ([trials-left trials]
             [trials-passed 0])
    (cond [(= trials-left 0)
           (println (format "trials passed:  ~a, trials ~a" trials-passed trials))
           (/ trials-passed trials)]
          [(experiment) (loop (- trials-left 1) (add1 trials-passed))]
          [else (loop (- trials-left 1) trials-passed)])))

;racket version for random-in-range
(define (random-in-range low high) 
  (let ((range (- high low))) 
    (+ low (* (random) range)))) 

(define test-pred
  (lambda (x y)
    (>= 9 (+ (sqr (- x 5))
            (sqr (- y 7))))))

;pick points at random within rectangle.
;point lies in region? if yes 
(define (estimate-integral pred x1 y1 x2 y2 trials)
  (let* ([experiment (lambda () (pred (random-in-range x1 x2) (random-in-range y1 y2)))]
         [area (abs (* (- x1 x2) (- y1 y2)))])
    (/ (* area (monte-carlo trials experiment)) 9)))

(exact->inexact (estimate-integral test-pred 2 4 8 10 10000))

;ex3.6
(define (some-rand-func x) (random 50))
(define some-val 0)
(define (rand command)
  (let ([internal-val some-val])
    (cond ([eq? command 'generate] (set! internal-val (some-rand-func internal-val))
                                   internal-val)
          ([eq? command 'reset] (lambda (x) (set! internal-val x))
                                internal-val))))
(check-equal? (rand 'reset) 0)

;ex3.7











  






  






  






  