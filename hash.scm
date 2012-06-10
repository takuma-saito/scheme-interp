
;; hash-table のクラスラッパー

(define-module hash
  (export p <hash> make-hash
          get put! exists? delete! clear! update! exmpty? map for-each filter))
(select-module hash)

(use utils)

(define-class <hash> ()
  ((table :init-keyword :table)))

(define-method initialize ((self <hash>) args)
  (set! (ref self 'table) (make-hash-table)))

(define-method p ((hash <hash>))
  (for-each
   (lambda (key value) (print #`"|,key|: |,value|"))
   hash))

(define (make-hash) (make <hash>))

(define (make-hash-args args)
  (update :h '(hash <hash>) args))

(define (make-hash-body method body)
  (define hash-object '(ref hash 'table))
  (cons method (cons hash-object body)))

(define-macro (define-hash-proxy name args body)
  (let* ((hash-method (+ "hash-table-" name))
         (body (make-hash-body hash-method (cdr body)))
         (args (make-hash-args args)))
    `(define-method ,name ,args
       ,body)))

(define-hash-proxy get (:h keyword)
  (:op keyword))

(define-hash-proxy put! (:h keyword value)
  (:op keyword value))

(define-hash-proxy exists? (:h keyword)
  (:op keyword))

(define-hash-proxy delete! (:h keyword)
  (:op keyword))

(define-hash-proxy clear! (:h)
  (:op))

(define-hash-proxy update! (:h keyword proc)
  (:op keyword proc))

(define-hash-proxy empty? (:h)
  (:op))

(define-hash-proxy map (proc :h)
  (:op proc))

(define-hash-proxy for-each (proc :h)
  (:op proc))

(define-method filter (proc (hash <hash>))
  (filter
   (map (lambda (x) (proc x)) (ref hash 'table))))
