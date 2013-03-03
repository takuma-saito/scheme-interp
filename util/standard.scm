

(define-module util.standard
  (use srfi-1)
  (export + first rest 1+ 1- length=1 length=2 wrap dump
          with-gensyms foreach main-return loop reserve-return
          update alias aliases))

(select-module util.standard)

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

(define-macro (dump var)
  `(let ((result ,var))
    (print result)
    result))

;; symbol をnamesで指定された名前の数だけ作成する
(define-macro (with-gensyms names . body)
  `(let (,@(map (lambda (name) `(,name (gensym))) names))
     ,@body))

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

;; next, break 付きのforeach
(define-macro (foreach proc lis)
  (with-gensyms
   (args)
   `(call/cc
     (lambda (break)
       (for-each
         (lambda ,args
           (call/cc
            (lambda (next)
              (apply ,proc ,args))))
         ,lis)))))

(define (alias-proc alias-name name)
  `(define ,alias-name ,name))

(define (begin-map . body)
  `(begin
     ,@(map proc)))

;; 関数に別名が付けられる
(define-macro (alias alias-name name)
  (alias-proc alias-name name))

;; 関数に別名が付けられる（複数形）
(define-macro (aliases . names)
  (begin-map
   (lambda (elem)
     (let ((alias-name (car elem)) (name (cadr elem)))
       (alias-proc alias-name name)))
   names))

(define-method filter ((lis <list>))
  (filter (lambda (x) x) lis))

(define-method update (keyword replace (elems <list>))
  (map (lambda (elem)
          (if (equal? elem keyword) replace elem)) elems))


