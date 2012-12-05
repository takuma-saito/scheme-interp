
(define-module env
  ;; *env* を公開するのはデバック用のため
  (export <env> frame-clear! frame-grow! frame-pop frame-top-level
          make-env lookup lookup-frame lookup-top-level bind! exists? update!
          bind-foreach))
(select-module env)

(use srfi-1)
(use srfi-11)
(use hash)
(use utils)

(define (make-frame) (make-hash))

(define (setup-frame)
  `(,(make-frame)))

(define-method empty? ((frames <list>))
  (equal? frames '()))

(define-method pop ((frames <list>))
  (if (empty? frames) #f
      (car frames)))

(define (call-with-search-keyword frame key cont)
  (if (exists? frame key)
      (cont (get frame key))
      #f))

(define (search-keyword frame key)
  (call-with-search-keyword frame key (lambda (value) value)))


;; public method

(define-macro (ref-env)
  `(ref self 'env))

(define-class <env> ()
  (env :init-keyword :env :init-value (setup-frame)))

(define (make-env) (make <env>))

(define-method frame-grow! ((self <env>))
  (set! ref-env (cons (make-frame) ref-env)))

(define-method frame-pop ((self <env>))
  (if (equal? (cdr ref-env) '()) #f
      (begin
        (set! ref-env (cdr ref-env))
        (car ref-env))))

(define-method frame-top-level ((self <env>))
  (last ref-env))

(define-method frame-clear! ((self <env>))
  (set! ref-env (setup-frame)))

(define-macro (where frames keyword search)
  `(begin
     (define (lookup-inner ,frames ,keyword)
       (cond [(empty? ,frames) #f]
             [(pop ,frames) => ,search]
             [else (lookup-inner (cdr ,frames) ,keyword)]))
     ))

(define-method lookup ((self <env>) keyword)
  (where ref-env keyword
         (lambda (frame) (search-keyword frame keyword))))

(define-method lookup-frame ((self <env>) keyword)
  (where ref-env keyword
         (lambda (frame)
           (call-with-search-keyword frame keyword (lambda (key) frame)))))

(define-method lookup-top-level ((self <env>) keyword)
  (let ((frame (last ref-env)))
    (search-keyword frame keyword)))

(define-method bind! ((self <env>) key value)
  (put! (car ref-env) key value))

(define-method exists? ((self <env>) key)
  (lookup self keyword))

(define-method update! ((self <env>) keyword value)
  (let ((result (lookup self keyword)))
    (if result
        (update! (lookup-frame self keyword) keyword value)
        #f)))

(define-method bind-foreach ((self <env>) lis)
  (for-each
   (lambda (elem)
     (let ((rest (if (length=2 elem) cadr cdr)))
         (bind! (car elem) (rest elem))))
   lis))

