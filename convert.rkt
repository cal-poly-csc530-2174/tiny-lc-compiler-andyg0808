#lang racket
(require rackunit)
(require racket/match)
(define (convert x) 
  (match x
    []
    [i (symbol->string i)]
    )
  )
(check-equal? (convert 'x) 'x)
(check-equal? (convert '2) '2)
