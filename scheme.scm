
;; scheme を記述するプログラム

;; (1) primitiveな型はそれ自身を返す (primitive -> 数, 文字列, nil)
;; (2) シンボルの評価値はその時点の環境で決まる
;; (3) nilでないリストの評価値は第0要素を関数, 第1要素以降を引数として関数適用する
;; (4) リスト要素の第0要素が特殊要素（if, beginなど）であった時は
;; 引数の評価順序をその特殊要素の指示する通りに行う
;; (5) マクロの場合は第一要素以下を評価せずにマクロの本体に渡し,
;; マクロ本体が返ってきた値を再度評価する

(use srfi-1)
(use module.console)

;; 各式のタグ情報
(define *forms* (make-hash-table))

(define (make-frame) (make-hash-table))

(define (append-string symbol string)
  (string->symbol (format "~a~a" (symbol->string symbol) string)))

(define (add-string symbol string)
  (string->symbol (format "~a~a" string (symbol->string symbol))))

(define first car)

;; name -> scheme-name エイリアス
(define-macro (scheme-alias . names)
  `(begin
     ,@(map (lambda (name) `(define ,(add-string name "scheme-") ,name))
            names)
     #f))

(scheme-alias apply eval)

(define scheme-apply apply)
(define scheme-eval eval)

(define (exp? exp target)
  (if (eq? (first exp) target) #t #f))

(define (dump-hash hash)
  (hash-table-for-each hash
                  (lambda (key value) (print #`"|,key|: |,value|"))))

(define-macro (main-return . body)
  `(call/cc
    (lambda (return)
      ,@body)))

(define-macro (get-form exp env)
  `(call/cc
    (lambda (return)
      (begin
        (cond 
         ,@(hash-table-map *forms*
                           (lambda (tag form)
                             `[(,(get-form-question form) ,exp ,env) ',tag]))
          [else (error "type error" ,exp)])))))

(define (get-form-question form) (car form))
(define (get-form-name form) (cadr form))

(define (make-question form)
  (get-form-qestion (hash-table-get *forms* form)))

(define (make-eval name)
  (get-form-name (hash-table-get *forms* name)))

(define (make-form-name name)
  (add-string name "form-"))

(define (make-question name)
  (append-string name "?"))

(define (make-form-question name)
  (make-form-name (make-question name)))

(define (primitive-name form) (cadr form))
(define (primitive-body form) (caddr form))

;; primitive 型用のタグを付ける
(define (add-primitive-tag form)
  (cons 'primitive form))

(define (add-primitive-tags forms)
  (map (lambda (form) (add-primitive-tag form)) forms))

;; primitive型かどうかを判定する
(define (primitive? exp)
  (if (pair? exp) (exp? exp 'primitive) #f))

;; primitiveな型かどうかを判定
(define (search-primitive exp env)
  (main-return
   (for-each
    (lambda (form)
      (let ((proc #?=(primitive-name #?=form)))
        (if (equal? proc exp) (return form))))
    env)
   #f))

;; (scheme-form タグ名 タグの真偽判断 タグの真偽判断が#tの場合に実行する式)
;; scheme-form ではexp, envの2変数がデフォルトで用意されている
;; 変換前
;; (scheme-form number
;;  (number? exp) exp)
;; 
;; 変換後
;; (define (number? exp env)
;;   (number? exp))
;; (define (number-form exp env)
;;   exp)
;; (set! *forms (cons 'number *forms*))
;; 
(define-macro (scheme-form name question-body patch . body)
  (define => 0)
  (if (not (equal? patch '=>)) (error (format "[~a]: ~a must be =>." name patch)))
  (let* ((question-name (make-form-question name))
         (form-name (make-form-name name)))
    `(begin
       (define (,question-name exp :optional env)
         ,question-body)
       (define (,form-name exp env)
         ,@body)
       (hash-table-put! *forms* ',name (list ,question-name ,form-name)))))

(define-macro (generate-form-question name)
  (let ((question (append-string name "?")))
  `(define (,question exp)
     (exp? exp ',name))))

(define-macro (generate-form-questions . names)
  `(begin
     ,@(map (lambda (name) `(generate-form-question ,name)) names)
     #t))

(define-macro (if-null exp null-exp body)
  `(if (null? ,exp) ,null-exp ,body))

(generate-form-questions if begin cond)

(define (generate-primitives)
  (define primitives
    `((car ,car)
      (cdr ,cdr)
      (+ ,+)
      (- ,-)
      (* ,*)
      (/ ,/)
      (> ,>)
      (< ,<)))
  (add-primitive-tags primitives))

;; 束縛変数を取得する
(define (search-variable exp env)
  (cond [(search-primitive exp env) => (lambda (primitive) primitive)]
        [else #f]))

;; 逐次的にexp内を評価する
(define (eval-sequence exp env)
  (last (eval-sequence-list exp env)))
                 
;; evalした後リストで返す
(define (eval-sequence-list exp env)
  (letrec ((make-lists
            (lambda (params)
              (if-null params '()
                       (cons (eval (car params) env)
                             (make-lists (cdr params)))))))
    (make-lists exp)))

(define (apply-primitive proc args)
  ((make-eval proc) (list proc args) *env*))
  
(define (apply proc args)
  (cond [(primitive? proc) (scheme-apply (primitive-body proc) args)]
        [(else (error "unknown procedure type: " proc))]))

(define (if-pred exp) (cadr exp))
(define (if-true exp) (caddr exp))
(define (if-false exp) (cadddr exp))
(define (true? exp) (if (equal? exp #f) #f #t))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (application? exp) (if (pair? exp) #t #f))

;; if 文
(scheme-form if (if? exp) =>
             (if (true? (eval (if-pred exp) env))
                 (eval (if-true exp) env)
                 (eval (if-false exp) env)))

;; begin文
(scheme-form begin (begin? exp) =>
             (eval-sequence (cdr exp) env))

;; apply
(scheme-form apply (application? exp) =>
             (apply (eval (operator exp) env)
                    (eval-sequence-list (operands exp) env)))

;; primitve
(scheme-form primitive (search-primitive #?=exp #?=env) =>
             (search-primitive exp env))

;; cond
;; (scheme-form cond (cond? exp) =>

;; define
             

(dump-hash *forms*)

(define (atom? exp)
  (or (number? exp) (string? exp) (boolean? exp)))

(define (variable? exp)
  (symbol? exp))

;; 全体を評価する
(define (eval exp env)
  (cond [(atom? exp) exp]
        [(variable? exp) (search-variable exp env)]
        [(get-form exp env) => (lambda (name) ((make-eval name) exp env))]
        [else (error "Unknown expression type: " type)]))

;; 環境
(define (setup-environment)
  (generate-primitives))

(define *env* (setup-environment))

(define (main)
  (print (console "my-scheme" (lambda (x) (eval x *env*)))))

(main)
