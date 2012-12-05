

;; 単体テストの構造
;; (:section section 10 :success 5 :error 5)

;; テスト結果を格納
(define *result* '())

(define (test-section name)
  (print (iota 1 20)))

(define (test-result))

(define-macro (test* name check . body)
  (let ((name (car args)))
  `(begin
     (define ,args ,@body)
     (cons (',name ,name)  *result*))))

