
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
(define (form-body exp) (cadr exp))
(define (form-tag exp) (car exp))
(define (application? exp) (if (pair? exp) #t #f))

(define (make-syntax-name name)
  (+ "syntax-" name))

;; primitive 型用のタグを付ける
(define (add-primitive-tag form)
  (cons :primitive (list form)))

;; primitive型かどうかを判定する
(define (primitive? exp)
  (if (pair? exp) (exp? exp :primitive) #f))

;; primitiveな型かどうかを判定
(define (search-primitive exp)
   (lookup-top-level exp))

;; (scheme-form タグ名 タグの真偽判断 タグの真偽判断が#tの場合に実行する式)

(define-macro (syntax-form name question-body => . body)
  (let ((form-name (make-syntax-name name)))
    `(begin
       (define (,form-name exp)
         ,@body)
       (bind! ',name (list :syntax ,form-name)))))

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
     (cons (first x) (add-primitive-tag (second x))))
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
  ((form-body (lookup-top-level (operator exp))) (operands exp)))

(define (apply-primitive proc args)
  ((make-eval proc) (list proc args)))
  
(define (apply proc args)
  (cond [(primitive? proc) (scheme-apply (form-body proc) args)]
        [(else (error "unknown procedure type: " proc))]))

;; if 文
(syntax-form if (if? exp) =>
             (if (true? (eval (if-pred exp)))
                 (eval (if-true exp))
                 (eval (if-false exp))))

;; begin文
(syntax-form begin (begin? exp) =>
             (eval-sequence (cdr exp)))

;; apply
(define (eval-application exp)
  (apply (eval (operator exp))
         (eval-sequence-list (operands exp))))

(define (atom? exp)
  (or (number? exp) (string? exp) (boolean? exp)))

;; 束縛変数を取得する
(define (search-variable exp)
  (cond [(search-primitive exp)]
        [else #f]))

(define (variable? exp)
  (cond [(symbol? exp) (search-variable exp)]
        [else #f]))

(define (syntax? exp)
  (main-return
   (for-each
    (lambda (name proc)
      (if (and (equal? (form-tag proc) :syntax) (equal? (operator exp) name))
          (return proc)))
      (frame-top-level))
   #f))

;; 全体を評価する
(define (eval exp)
  (cond [(atom? exp) exp]
        [(variable? exp)]
        [(syntax? exp) => (lambda (proc)
                            ((form-body proc) (operands exp)))]
        [(application? exp) (eval-application exp)]
        [else (error "Unknown expression type: " type)]))

;; 環境のセットアップ
(bind-foreach (setup-environment))
(p *env*)

(define (main args)
  (print (console "my-scheme" (lambda (x) (eval x)))))
