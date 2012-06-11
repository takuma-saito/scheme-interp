
(define-module tag
  (export <tag> p make-tag tag= tag body store get))
(select-module tag)

;; 環境へ登録する際に利用するタグ

(use hash)

(define tag-keyword :#tag)
(define body-keyword :#body)

(define-class <tag> ()
  ((table :init-keyword :table)))

(define-method initialize ((self <tag>) args)
  (next-method)
  (set! (ref self 'table) (make-hash)))

(define (make-tag . args)
  (let-keywords args ((name :name) (body :body))
                (let ((tag (make <tag>)))
                  (store tag tag-keyword name)
                  (store tag body-keyword body))))

(define-method p ((self <tag>))
  (p (ref self 'table)))

(define-method tag= ((self <tag>) keyword)
  (if (equal? keyword (get self tag-keyword)) #t #f))

(define-method tag ((self <tag>))
  (get (ref self 'table) tag-keyword))

(define-method body ((self <tag>))
  (get (ref self 'table) body-keyword))

(define-method store ((self <tag>) key value)
  (put! (ref self 'table) key value)
  self)

(define-method get ((self <tag>) key)
  (let ((table (ref self 'table)))
    (if (exists? table key)
        (get table key) #f)))
