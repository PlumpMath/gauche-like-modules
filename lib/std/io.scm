(define-module (std io)
  #:export (print println))

(define* (print #:rest args)
  (define (recur xs)
    (if (null? xs)
        #f
        (begin
          (display (car xs))
          (recur (cdr xs)))))
  (recur args))

(define* (println #:rest args)
  (define (recur xs)
    (if (null? xs)
        (newline)
        (begin
          (display (car xs))
          (recur (cdr xs)))))
  (recur args))
