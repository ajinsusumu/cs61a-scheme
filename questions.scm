(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define (cadar x) (car (cdr (car x))))
; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
  (map (lambda (x)
         (cons first x)
         )
       rests)
  )

(define (zip pairs)
  (cond
   ((null? pairs) '(()()))
   (else
    (
     (lambda (x)
      (cons
       (cons (caar pairs) (car x))
       (cons
        (cons (cadar pairs) (cadr x))
        nil))) (zip (cdr pairs)) )
   )
   )
  )

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17
  (define (enu c s)
    (cond
     ((null? s) nil)
     (else (cons (cons c (cons (car s) nil)) (enu (+ c 1) (cdr s))))
     )
    )
  (enu 0 s)
  )
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
  (cond
   ((null? denoms) nil)
   ((< total 1) '(()))
   ((< total (car denoms)) (list-change total (cdr denoms)))
   (else (append
          (cons-all (car denoms)
                    (list-change (- total (car denoms)) denoms)
                    )
          (list-change total (cdr denoms))
          )
         )
   )
  ; END PROBLEM 18
  )

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
  (cond ((atom? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (cons form
                 (cons params
                       (map let-to-lambda body)))
           ; END PROBLEM 19
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (let ((args (zip values)))
             (cons
                  (cons 'lambda
                        (cons (car args)
                              (map let-to-lambda body)))
                  (map let-to-lambda (cadr args)))))
           ; END PROBLEM 19
           )
        ;; ((= (car expr) '+)
        ;;  (cons (car expr)
        ;;        (map let-to-lambda (cdr expr))))
        (else
         ; BEGIN PROBLEM 19
         (cons (car expr)
               (map let-to-lambda (cdr expr)))
         ; END PROBLEM 19
         )))
