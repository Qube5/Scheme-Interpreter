(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
  (map (lambda (x) (cons first x)) rests)
)

(define (zip pairs)
  (if (eq? (car pairs) nil)
    nil
    (cons
      (map (lambda (x) (car x)) pairs)
      (zip (map (lambda (x) (cdr x)) pairs))
    )
  )
)

(define (enumerate s)
  ; BEGIN PROBLEM 17
  (define func
    (lambda (index list)
      (cond
        ((eq? list nil) nil)
        (else
          (cons
            (cons
              index
              (cons (car list) nil)
            )
            (func (+ index 1) (cdr list))
          )
        )
      )
    )
  )
  (func 0 s)
)
; END PROBLEM 17

(define (list-change total denoms)
  ; BEGIN PROBLEM 18
  (cond
    ( (= total 0) nil)
    ( (null? denoms) nil)
    ( (= (car denoms) total)
      (cons
        (cons
          (car denoms)
          nil
        )
        (list-change total (cdr denoms))
      )
    )
    ( (< (car denoms) total)
      (append
        (cons-all
          (car denoms)
          (list-change (- total (car denoms)) denoms)
        )
        (list-change total (cdr denoms))
      )
    )
    (else
      (list-change total (cdr denoms))
    )
  )
)
; END PROBLEM 18

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond
    ((atom? expr)
     ; BEGIN PROBLEM 19
      expr
     ; END PROBLEM 19
    )
    ((quoted? expr)
     ; BEGIN PROBLEM 19
      expr
     ; END PROBLEM 19
    )
    ((or
        (lambda? expr)
        (define? expr)
     )
     (let
       (
         (form   (car expr))
         (params (cadr expr))
         (body   (cddr expr))
       )
       ; BEGIN PROBLEM 19
        (cons
          form
          (cons
            params
            (let-to-lambda body)
          )
        )
       ; END PROBLEM 19
      )
    )
    ((let? expr)
     (let
       (
         (values (cadr expr))
         (body   (cddr expr))
       )
       ; BEGIN PROBLEM 19
       (cons
         (cons
           'lambda
           (cons
             (car (zip values))
             (let-to-lambda body)
           )
         )
         (map let-to-lambda (car (cdr (zip values))))
        )
       )
       ; END PROBLEM 19
    )
    (else
     ; BEGIN PROBLEM 19
      (map let-to-lambda expr)
     ; END PROBLEM 19
    )
  )
)
