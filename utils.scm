
(define-module utils
  (export + first rest 1+ 1- length=1 length=2 wrap
          main-return loop reserve-return update))

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

(define (length=1 lis)
  (= (length lis) 1))

(define (length=2 lis)
  (= (length lis) 2))

(define (wrap start end body)
  (format "~a ~a ~a" start body end))

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

;; return を予約する
(define-macro (reserve-return . body)
  `(let ((result #f))
     (let ((value
            (letrec ((return (lambda (val)
                               (set! result val))))
              ,@body)))
     (if result result value))))

(define-method filter ((lis <list>))
  (filter (lambda (x) x) lis))

(define-method update (keyword replace (elems <list>))
  (map (lambda (elem)
          (if (equal? elem keyword) replace elem)) elems))


