
(use srfi-1)

(get-keyword :section '(:section 10 :name 'takuma))

(define-class <kvs> ()
  (db :init-keywords :db))

(define-macro (ref-db)
  `(ref self 'db))

(define-method initialize ((self <kvs>) args)
  (next-method)
  (set! (ref self 'db) '()))

(define-method delete ())

(define-method get ())

(define-method update ((self <kvs>)))

(define-method exists? ())

(define-method insert ())

(define-method serialize ((self <kvs>) file))
