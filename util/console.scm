
(define-module module.console
  (export console))
(select-module module.console)

(use srfi-1)

;; 匿名モジュールを使う, R5RS enviroment, null enviroment
;; shared object

(define help "Help Message. (´・ω・`)")
(define exit-values '(exit quit e q))

;; 呼び出せる関数
(define (hello) "hello world")

;; プロンプトを表示させる
(define (show-prompt prompt)  
  (begin
    (display prompt)
    (flush)))

;; list に proc を適用する
;; 一つでもtrueがあれば処理をそこで終了しその値を返す
(define-macro (any proc proplist)
  `(call/cc
    (lambda (break)
      (begin
        (for-each
         (lambda ,(cadr proc) (if ,(caddr proc) (break #t) #f))
         ,proplist)
         #f))))

;; 終了条件
(define (exit-value? input)
  (any (lambda (x) (equal? x input)) exit-values))

(define (console prompt func)
  (loop
   (show-prompt #`",|prompt| > ")
   (print (let ((input (read)))
            (cond [(exit-value? input) (return "Bye Bye. (:->)")]
                  [else (func input)])))))

