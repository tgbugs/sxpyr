#lang racket
(let ([ids '()])
  #`(define proc-id
      #,@(let ([a 1])
           #`[(_ #,@(reverse ids))
              (proc-id #,@(reverse ids) def-expr)])))
