#lang racket

;; a neural network in racket

;; STARTING WITH LOGISTIC REGRESSION

;; regularization constant
(define R_CONST 0)

;; a F-List is a [List-of Number] : represents a list of features
;; a T-List is a [List-of Number] : represents our current hypothesis
;; a F-Set is a [List-of f-list] : a list of list of features IE data set

;; sigmoid : Number -> Number
;; compute the sigmoid function on a number
(define (sigmoid z)
  (/ 1 (+ 1 (exp (* -1 z)))))

;; hypothesize : T-List F-List -> Number
;; computes theta*X where theta is our guess vector and X is our features vector
;; theta and X must be of the same length
(define (hypothesize guess features)
  (cond [(not (= (length guess) (length features))) "vector dimensions do not match!"]
        [(empty? guess) 0]
        [(+ (* (first guess) (first features))
            (hypothesize (rest guess) (rest features)))]))


;; lr-cost : Number Number Number -> Number
;; computes regularized logistic regression cost for a hypothesis and actual
(define (lr-cost hypothesis actual l)
  (+ (- (* -1 actual (log (sigmoid hypothesis)))
        (* (- 1 actual) (log (- 1 (sigmoid hypothesis)))))
     (* (/ l 2) (* hypothesis hypothesis))))

;; hypothesize-all : F-Set T-List -> [List-of Number]
(define (hypothesize-all f-set thetas)
  (cond [(empty? f-set) '()]
        [else (cons (hypothesize thetas (first f-set))
                    (hypothesize-all (rest f-set) thetas))]))

;; total-lr-cost : F-Set T-List [List-of Number] -> Number
;; given a dataset and a theta-hypothesis, computer total average cost
(define (total-lr-cost f-set thetas actual)
  (/ (sum-cost (hypothesize-all f-set thetas) actual R_CONST) (length f-set)))

;; 

(define (sum-cost h actual l)
  (cond [(empty? h) 0]
        [else (+ (lr-cost (first h) (first actual) l)
                 (sum-cost (rest h) (rest actual) l))]))




