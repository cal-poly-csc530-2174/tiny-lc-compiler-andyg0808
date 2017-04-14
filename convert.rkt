#!/usr/bin/env racket
#lang racket
(require rackunit)
(require racket/match)
(require racket/format)
(define (convert x)
  (match x
    [`(λ (,id) ,lc) (format "function(~a) {return (~a)}" id (convert lc))]
    [`(println ,m)
     (format "(console.log(~a))"
             (convert m))]
    [`(ifleq0 ,guard ,then ,other)
     (format "(((~a) <= 0) ? (~a) : (~a))"
             (convert guard)
             (convert then)
             (convert other))]
    [`(,fun ,arg) (format "(~a)(~a)" (convert fun) (convert arg))]
    [`(,op ,l ,r) #:when (lambda ()
                           (case (op)
                             ((+ *) #t)
                             (else #f)))
     (format "((~a) ~a (~a))" (convert l) op (convert r))]
    [x      (cond
              [(number? x) (number->string x)]
              [(symbol? x) (symbol->string x)]
              [else (raise-arguments-error 'convert "Invalid syntax" "x" x)])]))
(module+ test
  (check-equal? (convert 'x) "x")
  (check-equal? (convert '2) "2")
  (check-equal? (convert '(λ (x) x)) "function(x) {return (x)}")
  (check-equal? (convert '((λ (x) x) 42)) "(function(x) {return (x)})(42)")
  (check-equal? (convert '(+ 1 2)) "(1 + 2)")
  (check-equal? (convert '(* 1 2)) "(1 * 2)")
  (check-equal? (convert '(ifleq0 2 3 4)) "if ((2) <= 0) {3} else {4}")
  (check-equal? (convert '(λ (a)
                            ((λ (x) (+ x 2))
                             (ifleq0
                              ((λ (x) (* 42 x)) a)
                              (* 4 a)
                              (+
                               ((λ (f) (* 11 f)) a)
                               3)))))
                "function(a) {return ((function(x) {return ((x + 2))})(if (((function(x) {return ((42 * x))})(a)) <= 0) {(4 * a)} else {((function(f) {return ((11 * f))})(a) + 3)}))}")
  (check-exn #px"Invalid syntax" (lambda () (convert '()))))
(module+ main
  (display (string-append (convert (read)) "\n")))
