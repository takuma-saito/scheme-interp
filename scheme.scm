
;; scheme を記述するプログラム

;; (1) primitiveな型はそれ自身を返す (primitive -> 数, 文字列, nil)
;; (2) シンボルの評価値はその時点の環境で決まる
;; (3) nilでないリストの評価値は第0要素を関数, 第1要素以降を引数として関数適用する
;; (4) リスト要素の第0要素が特殊要素（if, beginなど）であった時は
;; 引数の評価順序をその特殊要素の指示する通りに行う
;; (5) マクロの場合は第一要素以下を評価せずにマクロの本体に渡し,
;; マクロ本体が返ってきた値を再度評価する

(use srfi-1)
(use utils)
(use env)
(use hash)
(use debug)
(use tag)
(use module.console)

(define-macro (alias alias-name name)
  `(define ,alias-name ,name))

(define-macro (aliases . names)
  `(begin
    ,@(map (lambda (elem)
         (let ((alias-name (car elem)) (name (cadr elem)))
           `(define ,alias-name ,name)))
       names)))

(aliases
 (scheme-apply apply)
 (scheme-eval eval))

(define (exp? exp target)
  (if (eq? (first exp) target) #t #f))

(define (if-pred exp) (car exp))
(define (if-true exp) (cadr exp))
(define (if-false exp) (caddr exp))
(define (true? exp) (if (equal? exp #f) #f #t))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (application? exp) (if (pair? exp) #t #f))

(define (make-syntax-name name)
  (+ "syntax-" name))

;; primitive型かどうかを判定する
(define (primitive? exp)
  (if (pair? exp) (tag= exp :primitive) #f))

;; primitiveな型かどうかを判定
(define (search-primitive exp)
   (lookup-top-level exp))

(define-macro (syntax-form vars . body)
  (let* ((name (car vars))
         (args (cdr vars))
         (form-name (make-syntax-name name)))
    `(begin
       (define (,form-name ,@args)
         ,@body)
       (bind! ',name (make-tag :name :syntax :body ,form-name)))))

(define-macro (if-null exp null-exp body)
  `(if (null? ,exp) ,null-exp ,body))

(define (setup-environment)
  
  (define primitives
    `((car ,car)
      (cdr ,cdr)
      (+ ,+)
      (- ,-)
      (* ,*)
      (/ ,/)
      (> ,>)
      (< ,<)))
  
  (map
   (lambda (x)
     (cons (first x) (list (make-tag :name :primitive :body (second x)))))
   primitives))

;; 逐次的にexp内を評価する
(define (eval-sequence exp)
  (last (eval-sequence-list exp)))
                 
;; evalした後リストで返す
(define (eval-sequence-list exp)
  (letrec ((make-lists
            (lambda (params)
              (if-null params '()
                       (cons (eval (car params))
                             (make-lists (cdr params)))))))
    (make-lists exp)))

(define (eval-syntax exp)
  ((body (lookup-top-level (operator exp))) (operands exp)))

(define-macro (generate-tag-questions . names)
  `(begin
     ,@(map
        (lambda (name)
          (let ((keyword (make-keyword name))
                (question (+ name "?")))
            `(define (,question proc)
               (tag= proc ,keyword ))))
        names)
     #t))

(generate-tag-questions primitive closure)

(define (closure-params exp) (cadr exp))
(define (closure-body exp) (caddr exp))

(define (lambda-params exp) (car exp))
(define (lambda-body exp) (cadr exp))

(define (make-args params targets)
  (zip params
       (map
        (lambda (value)
          (make-tag :name :params :body value))
        targets)))

;; apply 文
(define (apply proc args)
  (reserve-return
   (cond [(primitive? proc) (scheme-apply (body proc) args)]
         [(closure? proc)
          (begin
            (frame-grow!)
            (bind-foreach (make-args (get proc :params) args))
            (return (eval (body proc)))
            (frame-pop))]
        [else (error "unknown procedure type: " proc)])))

;; if 文
(syntax-form (if exp)
  (if (true? (eval (if-pred exp)))
       (eval (if-true exp))
       (eval (if-false exp))))

;; begin文
(syntax-form (begin exp)
  (eval-sequence exp))

;; lambda 文
(syntax-form (lambda exp)
  (let ((tag (make-tag :name :closure :body (lambda-body exp))))
    (store tag :params (lambda-params exp))))

;; set!文
;; (syntax-form (set! exp)
;;   (bind! 

(define (eval-application exp)
  (apply (eval (operator exp))
         (eval-sequence-list (operands exp))))

(define (atom? exp)
  (or (number? exp) (string? exp) (boolean? exp)))

;; 束縛変数を取得する
(define (variable? exp)
  (if (symbol? exp)
      (cond [(search-primitive exp)]
            [(lookup exp) => (lambda (value) (body value))]
            [else #f])
      #f))

(define (syntax? exp)
  (main-return
   (for-each
    (lambda (name proc)
      (if (and (tag= proc :syntax) (equal? (operator exp) name))
          (return proc)))
      (frame-top-level))
   #f))

;; 全体を評価する
(define (eval exp)
  (cond [(atom? exp) exp]
        [(variable? exp)]
        [(syntax? exp) => (lambda (proc)
                            ((body proc) (operands exp)))]
        [(application? exp) (eval-application exp)]
        [else (error #`"Unknown expression type: ,exp" )]))

;; 環境のセットアップ
(bind-foreach (setup-environment))
(p (car *env*))

(define (main args)
  (print (console "my-scheme" (lambda (x) (eval x)))))

(eval '((lambda (square) (square 5)) (lambda (x) (* x x))))



