
(define-module utils
  (export + first rest 1+ 1- main-return loop update))

(select-module utils)

(define-method + ((symbol <symbol>) (string <string>))
  (string->symbol (format "~a~a" (symbol->string symbol) string)))

(define-method + ((string <string>) (symbol <symbol>))
  (string->symbol (format "~a~a" string (symbol->string symbol))))

(define first car)

(define rest cdr)

(define (1+ number)
  (+ number 1))

(define (1- number)
  (- number 1))

;; return が使える
(define-macro (main-return . body)
  `(call/cc
    (lambda (return)
      ,@body)))

;; return が使えるループ
(define-macro (loop . body)
  (main-return
    `(let loop ()
       ,@body
      (loop))))

(define-method filter ((lis <list>))
  (filter (lambda (x) x) lis))

(define-method update (keyword replace (elems <list>))
  (map (lambda (elem)
          (if (equal? elem keyword) replace elem)) elems))
